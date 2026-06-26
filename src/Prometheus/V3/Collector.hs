{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Collector (
    Collector (..),

    -- * Re-exports
    MetricName,
    Description,
    MetricType (..),
    Sample (..),
    SampleValue (..),
    defaultSample,
    ToSampleValue (..),
) where

import Prometheus.V3.Metric.Base (
    Description,
    MetricName,
    MetricType (..),
 )
import Prometheus.V3.Sample (
    Sample (..),
    SampleValue (..),
    ToSampleValue (..),
    defaultSample,
 )


data Collector = Collector
    { name :: MetricName
    , description :: Description
    , type_ :: MetricType
    , getSamples :: IO [Sample]
    }
