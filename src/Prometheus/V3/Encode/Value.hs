{-# LANGUAGE OverloadedStrings #-}

module Prometheus.V3.Encode.Value (
    showPromDouble,
    showPromText,
) where

import Data.Text (Text)
import qualified System.Metrics.Prometheus.Encode.Text.MetricId as V2


showPromDouble :: Double -> Text
showPromDouble = V2.textValue


showPromText :: Text -> Text
showPromText s = "\"" <> V2.escape s <> "\""
