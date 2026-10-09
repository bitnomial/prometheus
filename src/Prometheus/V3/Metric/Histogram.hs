{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- Remove after inlining V2.Histogram
{-# OPTIONS_GHC -Wno-orphans #-}

module Prometheus.V3.Metric.Histogram (
    V2.Histogram,
    register,
    new,

    -- * Specifying bounds
    linearBuckets,
    expBuckets,

    -- * Methods
    IsHistogram,
    reset,
    observe,
    sample,
    observeAndSample,

    -- ** Helpers
    time,

    -- * Re-exports
    labels,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import GHC.Stack (HasCallStack)
import Prometheus.V3.Label (toLabelValue)
import Prometheus.V3.Metric.Base
import Prometheus.V3.Metric.Labelled (labels)
import qualified Prometheus.V3.Registry as Registry
import Prometheus.V3.Sample
import qualified System.Metrics.Prometheus.Metric.Histogram as V2
import UnliftIO (MonadUnliftIO)


-- | An alias for @'Registry.register' (new ...)@.
--
-- Usages MUST be annotated with @OPAQUE@.
--
-- @
-- fooHistogram :: Prom.Histogram
-- fooHistogram = Histogram.register "foo_total" "Number of foos" [0, 1, 10]
-- {-# OPAQUE fooHistogram #-}
-- @
register :: MetricName -> Description -> [V2.UpperBound] -> V2.Histogram
register name description bounds = Registry.register $ new name description bounds
{-# INLINE register #-}


new ::
    MetricName ->
    Description ->
    [V2.UpperBound] ->
    Metric V2.Histogram
new name description bounds =
    Metric
        { name
        , description
        , initialize = liftIO $ V2.new bounds
        }


instance IsMetric V2.Histogram where
    getMetricType _ = MetricTypeHistogram
    getMetricSamples hist = do
        V2.HistogramSample{..} <- sample hist
        let sumSample = defaultSample{suffix = "_sum", value = toSampleValue histSum}
            countSample = defaultSample{suffix = "_count", value = toSampleValue histCount}
            bucketSamples =
                [ defaultSample
                    { suffix = "_bucket"
                    , labels = [("le", toLabelValue upperBound)]
                    , value = toSampleValue n
                    }
                | (upperBound, n) <- Map.toList histBuckets
                ]
        pure $ [sumSample, countSample] ++ bucketSamples


    isValidMetricLabel _ = \case
        "le" -> False -- https://prometheus.io/docs/instrumenting/writing_clientlibs/#histogram
        _ -> True


linearBuckets ::
    (HasCallStack) =>
    -- | Start
    V2.UpperBound ->
    -- | Width
    Double ->
    -- | Count
    Int ->
    [V2.UpperBound]
linearBuckets start width count
    | count < 1 = error "linearBuckets: count must be positive"
    | otherwise = [start + (width * fromIntegral i) | i <- [0 .. count - 1]]


expBuckets ::
    (HasCallStack) =>
    -- | Start
    V2.UpperBound ->
    -- | Factor
    Double ->
    -- | Count
    Int ->
    [V2.UpperBound]
expBuckets start factor count
    | count < 1 = error "expBuckets: count must be positive"
    | start <= 0 = error "expBuckets: start must be positive"
    | factor <= 1 = error "expBuckets: factor must be greater than 1"
    | otherwise = [start * (factor ** fromIntegral i) | i <- [0 .. count - 1]]


class IsHistogram h where
    getHistogram :: h -> IO V2.Histogram
instance IsHistogram V2.Histogram where
    getHistogram = pure
instance IsHistogram (IO V2.Histogram) where
    getHistogram = id


reset :: (IsHistogram h, MonadIO m) => h -> m ()
reset h = liftIO $ V2.reset =<< getHistogram h
{-# INLINE reset #-}


observe :: (IsHistogram h, MonadIO m) => h -> Double -> m ()
observe h x = liftIO $ V2.observe x =<< getHistogram h
{-# INLINE observe #-}


sample :: (IsHistogram h, MonadIO m) => h -> m V2.HistogramSample
sample h = liftIO $ V2.sample =<< getHistogram h
{-# INLINE sample #-}


observeAndSample :: (IsHistogram h, MonadIO m) => h -> Double -> m V2.HistogramSample
observeAndSample h x = liftIO $ V2.observeAndSample x =<< getHistogram h
{-# INLINE observeAndSample #-}


{----- Helpers -----}

time :: (IsHistogram h, MonadUnliftIO m) => h -> m a -> m a
time h = withDuration (observe h)
{-# INLINE time #-}
