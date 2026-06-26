{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Registry (
    Registry (..),
    new,
    globalRegistry,
    register,
    registerTo,
    unregisterFrom,
    ToCollector,

    -- * Sampling
    RegistrySample,
    sample,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Prometheus.V3.Collector (Collector (..))
import Prometheus.V3.Metric.Base (
    Description,
    IsMetric,
    Metric (..),
    MetricName,
    MetricType,
    getMetricSamples,
    getMetricType,
 )
import Prometheus.V3.Sample (Sample)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)


newtype Registry = Registry (IORef (Map MetricName Collector))


new :: (MonadIO m) => m Registry
new = Registry <$> newIORef Map.empty


globalRegistry :: Registry
globalRegistry = unsafePerformIO new
{-# OPAQUE globalRegistry #-}


class ToCollector a where
    type RegisterResult a
    getCollectorName :: a -> MetricName
    getCollectorDescription :: a -> Description
    getCollectorType :: a -> MetricType
    getCollectorSample :: a -> IO (IO [Sample], RegisterResult a)
instance ToCollector Collector where
    type RegisterResult Collector = Collector
    getCollectorName = (.name)
    getCollectorDescription = (.description)
    getCollectorType = (.type_)
    getCollectorSample collector = pure (collector.getSamples, collector)
instance (IsMetric a) => ToCollector (Metric a) where
    type RegisterResult (Metric a) = a
    getCollectorName = (.name)
    getCollectorDescription = (.description)
    getCollectorType _ = getMetricType (Proxy @a)
    getCollectorSample metric = do
        a <- metric.initialize
        pure (getMetricSamples a, a)


-- | Register the given metric with the global registry.
--
-- Only safe to use with top-level variables, which must be annotated with OPAQUE.
register :: (ToCollector a) => a -> RegisterResult a
register = unsafePerformIO . registerTo globalRegistry
{-# INLINE register #-}


-- | Register the given metric with the given registry.
registerTo :: (ToCollector a) => Registry -> a -> IO (RegisterResult a)
registerTo (Registry registryMapRef) a = do
    let name = getCollectorName a
        description = getCollectorDescription a
        type_ = getCollectorType a
    (getSamples, res) <- getCollectorSample a
    atomicModifyIORef' registryMapRef $ \registryMap ->
        (Map.insert name Collector{..} registryMap, ())
    pure res


-- | Unregister the given metric from the given registry.
unregisterFrom :: (ToCollector a) => Registry -> a -> IO ()
unregisterFrom (Registry registryMapRef) a = do
    atomicModifyIORef' registryMapRef $ \registryMap ->
        (Map.delete (getCollectorName a) registryMap, ())


type RegistrySample = Map MetricName (Collector, [Sample])


sample :: (MonadIO m) => Registry -> m RegistrySample
sample (Registry registryMapRef) = do
    registryMap <- readIORef registryMapRef
    liftIO . forM registryMap $ \collector -> do
        samples <- collector.getSamples
        -- TODO: revalidate labels with same logic as Labelled, for custom collectors
        pure (collector, samples)
