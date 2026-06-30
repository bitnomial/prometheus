{-# LANGUAGE DeriveDataTypeable #-}

module System.Metrics.Prometheus.Registry (
    Registry,
    RegistrySample (..),
    new,
    registerCounter,
    registerGauge,
    registerHistogram,
    listMetricIds,
    removeMetric,
    sample,
) where

import Control.Applicative ((<$>))
import Control.Exception (Exception, throw)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Typeable)

import System.Metrics.Prometheus.Metric (
    Metric (..),
    MetricSample (..),
 )
import System.Metrics.Prometheus.Metric.Counter (Counter)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import System.Metrics.Prometheus.Metric.Histogram (
    Histogram,
    UpperBound,
 )
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.MetricId (
    Labels (..),
    MetricId (MetricId),
    Name (..),
 )


newtype Registry = Registry {unRegistry :: Map MetricId Metric}
newtype RegistrySample = RegistrySample {unRegistrySample :: Map MetricId MetricSample}


newtype KeyError = KeyError MetricId deriving (Show, Typeable)
instance Exception KeyError


new :: Registry
new = Registry Map.empty


registerCounter :: Name -> Labels -> Registry -> IO (Counter, Registry)
registerCounter name labels registry = do
    counter <- Counter.new
    return (counter, Registry $ Map.insertWithKey collision mid (CounterMetric counter) (unRegistry registry))
  where
    mid = MetricId name labels
    collision k _ _ = throw (KeyError k)


registerGauge :: Name -> Labels -> Registry -> IO (Gauge Double, Registry)
registerGauge name labels registry = do
    gauge <- Gauge.new
    return (gauge, Registry $ Map.insertWithKey collision mid (GaugeMetric gauge) (unRegistry registry))
  where
    mid = MetricId name labels
    collision k _ _ = throw (KeyError k)


registerHistogram :: Name -> Labels -> [UpperBound] -> Registry -> IO (Histogram, Registry)
registerHistogram name labels buckets registry = do
    histogram <- Histogram.new buckets
    return (histogram, Registry $ Map.insertWithKey collision mid (HistogramMetric histogram) (unRegistry registry))
  where
    mid = MetricId name labels
    collision k _ _ = throw (KeyError k)


removeMetric :: MetricId -> Registry -> Registry
removeMetric i (Registry m) = Registry . Map.delete i $ m


listMetricIds :: Registry -> [MetricId]
listMetricIds = Map.keys . unRegistry


sample :: Registry -> IO RegistrySample
sample = fmap RegistrySample . mapM sampleMetric . unRegistry
  where
    sampleMetric :: Metric -> IO MetricSample
    sampleMetric (CounterMetric count) = CounterMetricSample <$> Counter.sample count
    sampleMetric (GaugeMetric gauge) = GaugeMetricSample <$> Gauge.sample gauge
    sampleMetric (HistogramMetric histogram) = HistogramMetricSample <$> Histogram.sample histogram
