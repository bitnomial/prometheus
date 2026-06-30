{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Prometheus.V3.Encode (
    encodeRegistrySample,

    -- * Options
    Options (..),
    EscapeScheme (..),
    defaultOptions,
) where

import Data.ByteString.Builder
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Prometheus.V3.Collector (Collector (..))
import Prometheus.V3.Encode.Value
import Prometheus.V3.Label (Label)
import Prometheus.V3.Metric.Base (
    MetricType (..),
 )
import Prometheus.V3.Name (EscapeScheme (..), showName)
import Prometheus.V3.Registry (RegistrySample)
import Prometheus.V3.Sample (
    Sample (..),
    SampleValue (..),
 )


data Options = Options
    { escapeScheme :: EscapeScheme
    }


defaultOptions :: Options
defaultOptions =
    Options
        { escapeScheme = EscapingUnderscores
        }


-- | Render 'RegistrySample' to Prometheus text format.
encodeRegistrySample :: Options -> RegistrySample -> Builder
encodeRegistrySample opts =
    unlines2
        . map (encodeCollectorSample opts)
        . Map.elems
  where
    unlines2 s = intercalate (newline <> newline) s <> newline


encodeCollectorSample :: Options -> (Collector, [Sample]) -> Builder
encodeCollectorSample opts (collector, samples) =
    intercalate newline $
        [ "# HELP " <> name <> space <> description
        , "# TYPE " <> name <> space <> type_
        , intercalate newline $ map (encodeSample opts name) samples
        ]
  where
    name = text $ showName opts.escapeScheme collector.name
    description = text collector.description
    type_ = encodeMetricType collector.type_


encodeSample :: Options -> Builder -> Sample -> Builder
encodeSample opts name sample =
    mconcat
        [ name <> text sample.suffix
        , encodeLabels opts sample.labels
        , space
        , encodeSampleValue sample.value
        ]


encodeLabels :: Options -> [Label] -> Builder
encodeLabels opts = \case
    [] -> mempty
    labels ->
        mconcat
            [ char8 '{'
            , intercalate (char8 ',') . map (encodeLabel opts) $ labels
            , char8 '}'
            ]


encodeLabel :: Options -> Label -> Builder
encodeLabel opts (name, value) =
    mconcat
        [ text $ showName opts.escapeScheme name
        , char8 '='
        , text $ showPromText value
        ]


encodeSampleValue :: SampleValue -> Builder
encodeSampleValue = \case
    SampleValueInt64 n -> int64Dec n
    SampleValueDouble n -> text $ showPromDouble n


encodeMetricType :: MetricType -> Builder
encodeMetricType = \case
    MetricTypeCounter -> "counter"
    MetricTypeGauge -> "gauge"
    MetricTypeHistogram -> "histogram"
    MetricTypeSummary -> "summary"
    MetricTypeUntyped -> "untyped"


{----- Helpers -----}

text :: Text -> Builder
text = byteString . Text.encodeUtf8


intercalate :: (Monoid a) => a -> [a] -> a
intercalate a = mconcat . List.intersperse a


newline :: Builder
newline = char8 '\n'


space :: Builder
space = char8 ' '
