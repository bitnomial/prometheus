{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
-- Remove after inlining V2.Gauge
{-# OPTIONS_GHC -Wno-orphans #-}

module Prometheus.V3.Metric.Gauge (
    V2.Gauge,
    register,
    new,
    newAt,

    -- * Methods
    IsGauge,
    IsNumGauge,
    inc,
    add,
    dec,
    sub,
    set,
    sample,
    modifyAndSample,

    -- ** Helpers
    setToCurrentTime,
    trackInProgress,
    time,

    -- * Re-exports
    labels,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prometheus.V3.Metric.Base
import Prometheus.V3.Metric.Labelled (labels)
import qualified Prometheus.V3.Registry as Registry
import Prometheus.V3.Sample
import Prometheus.V3.Utils.FromRealFrac (FromRealFrac, fromRealFrac)
import qualified System.Metrics.Prometheus.Metric.Gauge as V2
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket_)


-- | An alias for @'Registry.register' (new ...)@.
register :: (Num a, ToSampleValue a) => MetricName -> Description -> V2.Gauge a
register name description = Registry.register $ new name description
{-# INLINE register #-}


new :: (Num a) => MetricName -> Description -> Metric (V2.Gauge a)
new name description = newAt name description 0


newAt :: (Num a) => MetricName -> Description -> a -> Metric (V2.Gauge a)
newAt name description val =
    Metric
        { name
        , description
        , initialize = do
            gauge <- V2.new
            set gauge val
            pure gauge
        }


instance (SampleValueNum a) => IsMetric (V2.Gauge a) where
    getMetricType _ = MetricTypeGauge
    getMetricSamples gauge = do
        n <- sample gauge
        pure [defaultSample{value = toSampleValue n}]


class IsGauge g where
    type GaugeElem g
    getGauge :: g -> IO (V2.Gauge (GaugeElem g))
instance IsGauge (V2.Gauge a) where
    type GaugeElem (V2.Gauge a) = a
    getGauge = pure
instance IsGauge (IO (V2.Gauge a)) where
    type GaugeElem (IO (V2.Gauge a)) = a
    getGauge = id


type IsNumGauge g a = (IsGauge g, a ~ GaugeElem g, Num a)


inc :: (IsNumGauge g a, MonadIO m) => g -> m ()
inc g = liftIO $ V2.inc =<< getGauge g
{-# INLINE inc #-}


add :: (IsNumGauge g a, MonadIO m) => g -> a -> m ()
add g x = liftIO $ V2.add x =<< getGauge g
{-# INLINE add #-}


dec :: (IsNumGauge g a, MonadIO m) => g -> m ()
dec g = liftIO $ V2.dec =<< getGauge g
{-# INLINE dec #-}


sub :: (IsNumGauge g a, MonadIO m) => g -> a -> m ()
sub g x = liftIO $ V2.sub x =<< getGauge g
{-# INLINE sub #-}


set :: (IsNumGauge g a, MonadIO m) => g -> a -> m ()
set g x = liftIO $ V2.set x =<< getGauge g
{-# INLINE set #-}


sample :: (IsNumGauge g a, MonadIO m) => g -> m a
sample g = liftIO $ fmap V2.unGaugeSample . V2.sample =<< getGauge g
{-# INLINE sample #-}


modifyAndSample :: (IsNumGauge g a, MonadIO m) => g -> (a -> a) -> m a
modifyAndSample g f = liftIO $ fmap V2.unGaugeSample . V2.modifyAndSample f =<< getGauge g
{-# INLINE modifyAndSample #-}


{----- Helpers -----}

-- | Set to the current number of seconds since the epoch.
setToCurrentTime ::
    (IsGauge g, FromRealFrac (GaugeElem g), MonadIO m) =>
    g -> m ()
setToCurrentTime g = do
    now <- liftIO getPOSIXTime
    g' <- liftIO $ getGauge g
    set g' (fromRealFrac now)
{-# INLINE setToCurrentTime #-}


-- | Increment the gauge when the given action is running and decrement when it's exited.
trackInProgress :: (IsNumGauge g a, MonadUnliftIO m) => g -> m a -> m a
trackInProgress g = bracket_ (inc g) (dec g)
{-# INLINE trackInProgress #-}


-- | Set the gauge to the duration of the given action.
time ::
    (IsGauge g, FromRealFrac (GaugeElem g), MonadUnliftIO m) =>
    g -> m a -> m a
time g = withDuration (set g . fromRealFrac)
{-# INLINE time #-}
