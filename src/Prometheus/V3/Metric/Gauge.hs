{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified System.Metrics.Prometheus.Metric.Gauge as V2
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket_)


-- | An alias for @'Registry.register' (new ...)@.
register :: MetricName -> Description -> V2.Gauge
register name description = Registry.register $ new name description
{-# INLINE register #-}


new :: MetricName -> Description -> Metric V2.Gauge
new name description = newAt name description 0


newAt :: MetricName -> Description -> Double -> Metric V2.Gauge
newAt name description val =
    Metric
        { name
        , description
        , initialize = do
            gauge <- V2.new
            set gauge val
            pure gauge
        }


instance IsMetric V2.Gauge where
    getMetricType _ = MetricTypeGauge
    getMetricSamples gauge = do
        n <- sample gauge
        pure [defaultSample{value = toSampleValue n}]


class IsGauge g where
    getGauge :: g -> IO V2.Gauge
instance IsGauge V2.Gauge where
    getGauge = pure
instance IsGauge (IO V2.Gauge) where
    getGauge = id


inc :: (IsGauge g, MonadIO m) => g -> m ()
inc g = liftIO $ V2.inc =<< getGauge g
{-# INLINE inc #-}


add :: (IsGauge g, MonadIO m) => g -> Double -> m ()
add g x = liftIO $ V2.add x =<< getGauge g
{-# INLINE add #-}


dec :: (IsGauge g, MonadIO m) => g -> m ()
dec g = liftIO $ V2.dec =<< getGauge g
{-# INLINE dec #-}


sub :: (IsGauge g, MonadIO m) => g -> Double -> m ()
sub g x = liftIO $ V2.sub x =<< getGauge g
{-# INLINE sub #-}


set :: (IsGauge g, MonadIO m) => g -> Double -> m ()
set g x = liftIO $ V2.set x =<< getGauge g
{-# INLINE set #-}


sample :: (IsGauge g, MonadIO m) => g -> m Double
sample g = liftIO $ fmap V2.unGaugeSample . V2.sample =<< getGauge g
{-# INLINE sample #-}


modifyAndSample :: (IsGauge g, MonadIO m) => g -> (Double -> Double) -> m Double
modifyAndSample g f = liftIO $ fmap V2.unGaugeSample . V2.modifyAndSample f =<< getGauge g
{-# INLINE modifyAndSample #-}


{----- Helpers -----}

-- | Set to the current number of seconds since the epoch.
setToCurrentTime :: (IsGauge g, MonadIO m) => g -> m ()
setToCurrentTime g = do
    now <- liftIO getPOSIXTime
    g' <- liftIO $ getGauge g
    set g' (realToFrac now)
{-# INLINE setToCurrentTime #-}


-- | Increment the gauge when the given action is running and decrement when it's exited.
trackInProgress :: (IsGauge g, MonadUnliftIO m) => g -> m a -> m a
trackInProgress g = bracket_ (inc g) (dec g)
{-# INLINE trackInProgress #-}


-- | Set the gauge to the duration of the given action.
time :: (IsGauge g, MonadUnliftIO m) => g -> m a -> m a
time g = withDuration (set g)
{-# INLINE time #-}
