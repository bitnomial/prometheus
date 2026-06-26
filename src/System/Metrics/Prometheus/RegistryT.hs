{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Metrics.Prometheus.RegistryT where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.State.Strict (
    StateT (..),
    evalStateT,
    execStateT,
    get,
 )

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
    Registry,
    RegistrySample,
    new,
 )
import qualified System.Metrics.Prometheus.Registry as R


newtype RegistryT m a = RegistryT {unRegistryT :: StateT Registry m a}
    deriving (Monad, MonadTrans, Applicative, Functor, MonadIO)


evalRegistryT :: Monad m => RegistryT m a -> m a
evalRegistryT = flip evalStateT new . unRegistryT


execRegistryT :: Monad m => RegistryT m a -> m Registry
execRegistryT = flip execStateT new . unRegistryT


runRegistryT :: Monad m => RegistryT m a -> m (a, Registry)
runRegistryT = flip runStateT new . unRegistryT


withRegistry :: MonadIO m => (Registry -> m (a, Registry)) -> RegistryT m a
withRegistry = RegistryT . StateT


registerCounter :: MonadIO m => Name -> Labels -> RegistryT m Counter
registerCounter n l = withRegistry (liftIO . R.registerCounter n l)


registerGauge :: MonadIO m => Name -> Labels -> RegistryT m (Gauge Double)
registerGauge n l = withRegistry (liftIO . R.registerGauge n l)


registerHistogram :: MonadIO m => Name -> Labels -> [Histogram.UpperBound] -> RegistryT m Histogram
registerHistogram n l u = withRegistry (liftIO . R.registerHistogram n l u)


removeMetric :: MonadIO m => MetricId -> RegistryT m ()
removeMetric i = withRegistry (pure . (,) () . R.removeMetric i)


listMetricIds :: MonadIO m => RegistryT m [MetricId]
listMetricIds = R.listMetricIds <$> RegistryT get


sample :: Monad m => RegistryT m (IO RegistrySample)
sample = R.sample <$> RegistryT get
