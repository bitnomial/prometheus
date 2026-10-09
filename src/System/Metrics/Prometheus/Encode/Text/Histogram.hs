{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Encode.Text.Histogram (
    encodeHistogram,
) where

import Data.ByteString.Builder (Builder)
import Data.List (intersperse)
import qualified Data.Map as Map

import System.Metrics.Prometheus.Encode.Text.MetricId (
    encodeDouble,
    encodeInt,
    encodeLabels,
    encodeName,
    newline,
    space,
    textValue,
 )
import System.Metrics.Prometheus.Metric.Histogram (
    HistogramSample (..),
    UpperBound,
 )
import System.Metrics.Prometheus.MetricId (
    MetricId (..),
    addLabel,
 )


encodeHistogram :: MetricId -> HistogramSample -> Builder
encodeHistogram mid histogram =
    encodeHistogramBuckets mid histogram <> newline
        <> n
        <> "_sum"
        <> ls
        <> space
        <> encodeDouble (histSum histogram)
        <> newline
        <> n
        <> "_count"
        <> ls
        <> space
        <> encodeInt (histCount histogram)
  where
    n = encodeName $ name mid
    ls = encodeLabels $ labels mid


encodeHistogramBuckets :: MetricId -> HistogramSample -> Builder
encodeHistogramBuckets mid =
    mconcat . intersperse newline . map snd . Map.toList
        . Map.mapWithKey (encodeHistogramBucket mid)
        . histBuckets


encodeHistogramBucket :: MetricId -> UpperBound -> Double -> Builder
encodeHistogramBucket mid upperBound count =
    encodeName (name mid) <> "_bucket" <> encodeLabels labels' <> space <> encodeDouble count
  where
    labels' = addLabel "le" (textValue upperBound) (labels mid)
