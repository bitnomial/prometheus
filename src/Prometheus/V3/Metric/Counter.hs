{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
-- Remove after inlining V2.Counter
{-# OPTIONS_GHC -Wno-orphans #-}

module Prometheus.V3.Metric.Counter (
    V2.Counter,
    V2.Counter',
    register,
    new,

    -- * Methods
    IsCounter,
    IsNumCounter,
    inc,
    add,
    reset,
    sample,
    addAndSample,

    -- ** Helpers
    countExceptions,
    countExceptionsWhere,

    -- * Re-exports
    labels,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prometheus.V3.Metric.Base
import Prometheus.V3.Metric.Labelled (labels)
import qualified Prometheus.V3.Registry as Registry
import Prometheus.V3.Sample
import qualified System.Metrics.Prometheus.Metric.Counter as V2
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (Exception, withException)


-- | An alias for @'Registry.register' (new ...)@.
register ::
    (Num a, Ord a, ToSampleValue a, V2.HasCounterBackend a) =>
    MetricName -> Description -> (V2.Counter' a)
register name description = Registry.register $ new name description
{-# INLINE register #-}


new ::
    (Num a, V2.HasCounterBackend a) =>
    MetricName -> Description -> Metric (V2.Counter' a)
new name description =
    Metric
        { name
        , description
        , initialize = liftIO V2.new
        }


instance (SampleValueNum a, Ord a, V2.HasCounterBackend a) => IsMetric (V2.Counter' a) where
    getMetricType _ = MetricTypeCounter
    getMetricSamples counter = do
        n <- sample counter
        pure [defaultSample{value = toSampleValue n}]


class IsCounter c where
    type CounterElem c
    getCounter :: c -> IO (V2.Counter' (CounterElem c))
instance IsCounter (V2.Counter' a) where
    type CounterElem (V2.Counter' a) = a
    getCounter = pure
instance IsCounter (IO (V2.Counter' a)) where
    type CounterElem (IO (V2.Counter' a)) = a
    getCounter = id


type IsNumCounter c a =
    ( IsCounter c
    , a ~ CounterElem c
    , V2.HasCounterBackend a
    , Num a
    , Ord a
    )


inc :: (IsNumCounter c a, MonadIO m) => c -> m ()
inc c = liftIO $ V2.inc =<< getCounter c
{-# INLINE inc #-}


add :: (IsNumCounter c a, MonadIO m) => c -> a -> m ()
add c n = liftIO $ V2.add n =<< getCounter c
{-# INLINE add #-}


reset :: (IsNumCounter c a, MonadIO m) => c -> m ()
reset c = liftIO $ V2.set 0 =<< getCounter c
{-# INLINE reset #-}


sample :: (IsNumCounter c a, MonadIO m) => c -> m a
sample c = liftIO $ fmap V2.unCounterSample . V2.sample =<< getCounter c
{-# INLINE sample #-}


addAndSample :: (IsNumCounter c a, MonadIO m) => c -> a -> m a
addAndSample c n = liftIO $ fmap V2.unCounterSample . V2.addAndSample n =<< getCounter c
{-# INLINE addAndSample #-}


{----- Helpers -----}

-- | Count the number of times a particular exception is thrown.
countExceptions ::
    forall e c m a.
    (IsNumCounter c a, Exception e, MonadUnliftIO m) =>
    c -> m a -> m a
countExceptions c = countExceptionsWhere c (\(_ :: e) -> True)
{-# INLINE countExceptions #-}


-- | Like 'countExceptions', except provide a function to decide whether to
-- include the exception in the count.
countExceptionsWhere ::
    forall e c m a.
    (IsNumCounter c a, Exception e, MonadUnliftIO m) =>
    c -> (e -> Bool) -> m a -> m a
countExceptionsWhere c f io = io `withException` \e -> when (f e) (inc c)
{-# INLINE countExceptionsWhere #-}
