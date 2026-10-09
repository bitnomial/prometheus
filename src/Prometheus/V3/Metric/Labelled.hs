{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Metric.Labelled (
    Labelled (..),
    withLabels,
    labels,
) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy (..))
import Data.Tuple (swap)
import Prometheus.V3.Label (
    IsLabelValueTuple,
    LabelTupleNames,
    toLabelNameList,
    toLabelValueList,
 )
import Prometheus.V3.Metric.Base
import Prometheus.V3.Sample (Sample (..))
import UnliftIO.Exception (SomeException, mask, throwIO, trySyncOrAsync)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, readMVar)


data Labelled l a = Labelled
    { labelNames :: LabelTupleNames l
    , initChild :: IO a
    , metricMapRef :: IORef (HashMap l (InitState a))
    }


data InitState a
    = Init (MVar (Either SomeException a))
    | Ready !a


withLabels ::
    forall l a.
    (IsMetric a, IsLabelValueTuple l) =>
    LabelTupleNames l ->
    [l] ->
    Metric a ->
    Metric (Labelled l a)
withLabels labelNames initialLabels metric =
    validateLabelNames
        metric
            { initialize = do
                metricMap <-
                    fmap HashMap.fromList . sequence $
                        [ (vals,) . Ready <$> metric.initialize
                        | vals <- initialLabels
                        ]
                metricMapRef <- newIORef metricMap
                pure
                    Labelled
                        { labelNames
                        , initChild = metric.initialize
                        , metricMapRef
                        }
            }
  where
    -- TODO: error on duplicate labels
    validateLabelNames =
        case filter (not . isValid) (toLabelNameList labelNames) of
            [] -> id
            invalidNames -> error $ "Invalid label names: " <> show invalidNames

    isValid = isValidMetricLabel (Proxy @a)


labels :: (Hashable l) => l -> Labelled l a -> IO a
labels labelVals labelled = mask $ \unmask -> do
    initVar0 <- newEmptyMVar
    initState <-
        atomicModifyIORef' labelled.metricMapRef $
            insertIfMissing labelVals (Init initVar0)
    case initState of
        -- Child is already initialized
        Just (Ready child) -> pure child
        -- Child is being initialized right now in another thread
        Just (Init initVar) -> either throwIO pure =<< readMVar initVar
        -- Child needs initialization
        Nothing -> do
            result <- trySyncOrAsync $ unmask labelled.initChild
            -- Update map for future readers
            atomicModifyIORef' labelled.metricMapRef $ \metricMap ->
                let f =
                        case result of
                            Right child -> HashMap.insert labelVals (Ready child)
                            Left _ -> HashMap.delete labelVals
                 in (f metricMap, ())
            -- Unblock waiting threads
            putMVar initVar0 result
            -- Return result for this thread
            either throwIO pure result
  where
    insertIfMissing :: (Hashable k) => k -> v -> HashMap k v -> (HashMap k v, Maybe v)
    insertIfMissing k v =
        let f = \case
                Just v' -> (Just v', Just v')
                Nothing -> (Nothing, Just v)
         in swap . HashMap.alterF f k


instance (IsMetric a, IsLabelValueTuple l) => IsMetric (Labelled l a) where
    getMetricType _ = getMetricType (Proxy @a)
    getMetricSamples labelled = do
        metricMap <- readIORef labelled.metricMapRef
        labelledSamples <-
            sequence
                [ (labelVals,) <$> getMetricSamples child
                | (labelVals, Ready child) <- HashMap.toList metricMap
                ]
        pure
            [ sample
                { labels = zip labelNameList (toLabelValueList labelVals) <> sample.labels
                }
            | (labelVals, samples) <- labelledSamples
            , sample <- samples
            ]
      where
        labelNameList = toLabelNameList labelled.labelNames
