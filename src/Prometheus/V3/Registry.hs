{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
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

    -- * getOrRegister
    getOrRegister,
    getOrRegisterTo,

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
import Prometheus.V3.Utils.InitMap (InitMap)
import qualified Prometheus.V3.Utils.InitMap as InitMap
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep (..), TypeRep, Typeable, eqTypeRep, typeOf, type (:~~:) (HRefl))
import UnliftIO.Exception (Exception, impureThrow, throwIO)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)


newtype Registry = Registry (IORef (InitMap MetricName SomeCollector))


data SomeCollector
    = forall a.
      SomeCollector
    { collector :: Collector
    , typeRep :: TypeRep a
    , result :: RegisterResult a
    }


new :: (MonadIO m) => m Registry
new = Registry <$> newIORef InitMap.empty


globalRegistry :: Registry
globalRegistry = unsafePerformIO new
{-# OPAQUE globalRegistry #-}


class (Typeable a) => ToCollector a where
    type RegisterResult a
    getCollectorName :: a -> MetricName
    getCollectorDescription :: a -> Description
    getCollectorType :: a -> MetricType
    initCollectorResult :: a -> IO (RegisterResult a)
    getCollectorSample :: a -> RegisterResult a -> IO [Sample]
instance ToCollector Collector where
    type RegisterResult Collector = Collector
    getCollectorName = (.name)
    getCollectorDescription = (.description)
    getCollectorType = (.type_)
    initCollectorResult = pure
    getCollectorSample _ = (.getSamples)
instance (Typeable a, IsMetric a) => ToCollector (Metric a) where
    type RegisterResult (Metric a) = a
    getCollectorName = (.name)
    getCollectorDescription = (.description)
    getCollectorType _ = getMetricType (Proxy @a)
    initCollectorResult = (.initialize)
    getCollectorSample _ = getMetricSamples


-- | Register the given metric with the global registry.
--
-- Only safe to use with top-level variables, which must be annotated with @OPAQUE@.
register :: (ToCollector a) => a -> RegisterResult a
register = unsafePerformIO . registerTo globalRegistry
{-# INLINE register #-}


-- | Register the given metric with the given registry.
registerTo :: (ToCollector a) => Registry -> a -> IO (RegisterResult a)
registerTo registry a =
    getOrRegisterTo' registry a >>= \case
        InitMap.Created res -> pure res
        InitMap.Existed _ -> throwIO (MetricRegisteredMultipleTimesError name)
  where
    name = getCollectorName a


getOrRegister :: (ToCollector a) => a -> IO (RegisterResult a)
getOrRegister = getOrRegisterTo globalRegistry


getOrRegisterTo :: (ToCollector a) => Registry -> a -> IO (RegisterResult a)
getOrRegisterTo registry = fmap InitMap.fromGetOrCreateResult . getOrRegisterTo' registry


getOrRegisterTo' :: forall a. (ToCollector a) => Registry -> a -> IO (InitMap.GetOrCreateResult (RegisterResult a))
getOrRegisterTo' (Registry registryMapRef) a =
    fmap getResult <$> InitMap.getOrCreate' name initCollector registryMapRef
  where
    inputType = typeOf a
    name = getCollectorName a
    initCollector = do
        result <- initCollectorResult a
        pure
            SomeCollector
                { collector =
                    Collector
                        { name
                        , description = getCollectorDescription a
                        , type_ = getCollectorType a
                        , getSamples = getCollectorSample a result
                        }
                , typeRep = inputType
                , result
                }

    getResult :: SomeCollector -> RegisterResult a
    getResult SomeCollector{typeRep, result} =
        case eqTypeRep typeRep inputType of
            Just HRefl -> result
            Nothing -> impureThrow $ MetricRegisteredDifferentTypeError name (SomeTypeRep inputType)


newtype MetricRegisteredMultipleTimesError = MetricRegisteredMultipleTimesError MetricName
    deriving (Show)
instance Exception MetricRegisteredMultipleTimesError


data MetricRegisteredDifferentTypeError = MetricRegisteredDifferentTypeError MetricName SomeTypeRep
    deriving (Show)
instance Exception MetricRegisteredDifferentTypeError


-- | Unregister the given metric from the given registry.
unregisterFrom :: (ToCollector a) => Registry -> a -> IO ()
unregisterFrom (Registry registryMapRef) a = do
    atomicModifyIORef' registryMapRef $ \registryMap ->
        (InitMap.delete (getCollectorName a) registryMap, ())


type RegistrySample = Map MetricName (Collector, [Sample])


sample :: (MonadIO m) => Registry -> m RegistrySample
sample (Registry registryMapRef) = do
    registryMap <- Map.fromList . InitMap.toList <$> readIORef registryMapRef
    liftIO . forM registryMap $ \SomeCollector{collector} -> do
        samples <- collector.getSamples
        -- TODO: revalidate labels with same logic as Labelled, for custom collectors
        pure (collector, samples)
