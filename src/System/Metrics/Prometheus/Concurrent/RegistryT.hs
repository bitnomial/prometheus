{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Metrics.Prometheus.Concurrent.RegistryT where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (
    ReaderT (..),
    ask,
 )

import System.Metrics.Prometheus.Concurrent.Registry (Registry, new)
import qualified System.Metrics.Prometheus.Concurrent.Registry as R
import System.Metrics.Prometheus.Metric.Counter (Counter)
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import System.Metrics.Prometheus.Metric.Histogram (Histogram)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.MetricId (
    Labels,
    MetricId,
    Name,
 )
import System.Metrics.Prometheus.Registry (
    RegistrySample,
    listMetricIds,
 )


newtype RegistryT m a = RegistryT {unRegistryT :: ReaderT Registry m a}
    deriving (Monad, MonadTrans, Applicative, Functor, MonadIO)


runRegistryT :: MonadIO m => RegistryT m a -> m a
runRegistryT registry = liftIO new >>= runReaderT (unRegistryT registry)


registerCounter :: MonadIO m => Name -> Labels -> RegistryT m Counter
registerCounter n l = RegistryT ask >>= liftIO . R.registerCounter n l


registerGauge :: MonadIO m => Name -> Labels -> RegistryT m (Gauge Double)
registerGauge n l = RegistryT ask >>= liftIO . R.registerGauge n l


registerHistogram :: MonadIO m => Name -> Labels -> [Histogram.UpperBound] -> RegistryT m Histogram
registerHistogram n l b = RegistryT ask >>= liftIO . R.registerHistogram n l b


removeMetric :: MonadIO m => MetricId -> RegistryT m ()
removeMetric i = RegistryT ask >>= liftIO . R.removeMetric i


listMetricIds :: MonadIO m => RegistryT m [MetricId]
listMetricIds = RegistryT ask >>= liftIO . R.listMetricIds


sample :: Monad m => RegistryT m (IO RegistrySample)
sample = R.sample <$> RegistryT ask
