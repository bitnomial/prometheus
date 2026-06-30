{-# LANGUAGE DisambiguateRecordFields #-}
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
import Data.Proxy (Proxy (..))
import Prometheus.V3.Label (
    IsLabelValueTuple,
    LabelTupleNames,
    toLabelNameList,
    toLabelValueList,
 )
import Prometheus.V3.Metric.Base
import Prometheus.V3.Sample (Sample (..))
import UnliftIO.MVar (MVar, modifyMVar, newMVar, readMVar)


data Labelled l a = Labelled
    { labelNames :: LabelTupleNames l
    , initChild :: IO a
    , metricMapVar :: MVar (HashMap l a)
    }


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
                        [ (vals,) <$> metric.initialize
                        | vals <- initialLabels
                        ]
                metricMapVar <- newMVar metricMap
                pure
                    Labelled
                        { labelNames
                        , initChild = metric.initialize
                        , metricMapVar
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
labels labelVals labelled =
    modifyMVar labelled.metricMapVar $ \metricMap -> do
        case HashMap.lookup labelVals metricMap of
            Just child -> pure (metricMap, child)
            Nothing -> do
                child <- labelled.initChild
                pure (HashMap.insert labelVals child metricMap, child)


instance (IsMetric a, IsLabelValueTuple l) => IsMetric (Labelled l a) where
    getMetricType _ = getMetricType (Proxy @a)
    getMetricSamples labelled = do
        metricMap <- readMVar labelled.metricMapVar
        samplesMap <- traverse getMetricSamples metricMap
        pure
            [ sample
                { labels = zip labelNameList (toLabelValueList labelVals) <> sample.labels
                }
            | (labelVals, samples) <- HashMap.toList samplesMap
            , sample <- samples
            ]
      where
        labelNameList = toLabelNameList labelled.labelNames
