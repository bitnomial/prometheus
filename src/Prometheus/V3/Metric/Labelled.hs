{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Metric.Labelled (
    Labelled (..),
    withLabels,
    IsLabelled (..),
    labels,
) where

import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Prometheus.V3.Label (
    IsLabelValueTuple,
    LabelTupleNames,
    toLabelNameList,
    toLabelValueList,
 )
import Prometheus.V3.Metric.Base
import qualified Prometheus.V3.Registry as Registry
import Prometheus.V3.Sample (Sample (..))
import Prometheus.V3.Utils.InitMap (InitMap)
import qualified Prometheus.V3.Utils.InitMap as InitMap


data Labelled label a = Labelled
    { labelNames :: LabelTupleNames label
    , initChild :: IO a
    , metricMapRef :: IORef (InitMap label a)
    }


withLabels ::
    forall label a.
    (IsMetric a, IsLabelValueTuple label) =>
    LabelTupleNames label ->
    [label] ->
    Metric a ->
    Metric (Labelled label a)
withLabels labelNames initialLabels metric =
    validateLabelNames
        metric
            { initialize = do
                metricMap <-
                    fmap InitMap.fromList . sequence $
                        [ (vals,) <$> metric.initialize
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


class IsLabelled l where
    type ToLabelledLabel l
    type ToLabelledElem l
    load :: l -> IO (Labelled (ToLabelledLabel l) (ToLabelledElem l))
instance IsLabelled (Labelled l a) where
    type ToLabelledLabel (Labelled l a) = l
    type ToLabelledElem (Labelled l a) = a
    load = pure
instance (IsMetric a, IsLabelValueTuple l, Typeable l, Typeable a) => IsLabelled (Metric (Labelled l a)) where
    type ToLabelledLabel (Metric (Labelled l a)) = l
    type ToLabelledElem (Metric (Labelled l a)) = a
    load = Registry.getOrRegister


labels ::
    ( Hashable label
    , IsLabelled l
    , ToLabelledLabel l ~ label
    , ToLabelledElem l ~ a
    ) =>
    label -> l -> IO a
labels labelVals labelled = do
    Labelled{..} <- load labelled
    InitMap.getOrCreate labelVals initChild metricMapRef


instance (IsMetric a, IsLabelValueTuple label) => IsMetric (Labelled label a) where
    getMetricType _ = getMetricType (Proxy @a)
    getMetricSamples labelled = do
        metricMap <- readIORef labelled.metricMapRef
        labelledSamples <-
            sequence
                [ (labelVals,) <$> getMetricSamples child
                | (labelVals, child) <- InitMap.toList metricMap
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
