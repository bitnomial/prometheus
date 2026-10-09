{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Encode.Text.MetricId (
    encodeHeader,
    encodeMetricId,
    encodeLabels,
    encodeName,
    textValue,
    encodeDouble,
    encodeInt,
    escape,
    newline,
    space,
) where

import Data.ByteString.Builder (
    Builder,
    byteString,
    char8,
    intDec,
 )
import Data.List (intersperse)
import Data.Text (Text, replace)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.RealFloat (
    FPFormat (Generic),
    formatRealFloat,
 )
import Prelude hiding (null)

import System.Metrics.Prometheus.Metric (
    MetricSample (..),
    metricSample,
 )
import System.Metrics.Prometheus.MetricId (
    Labels (..),
    MetricId (..),
    Name (..),
    null,
    toList,
 )


encodeHeader :: MetricId -> MetricSample -> Builder
encodeHeader mid sample =
    "# TYPE " <> nm <> space <> encodeSampleType sample
  where
    -- <> "# HELP " <> nm <> space <> escape "help" <> newline <>
    nm = encodeName (name mid)


encodeSampleType :: MetricSample -> Builder
encodeSampleType =
    byteString
        . metricSample
            (const "counter")
            (const "gauge")
            (const "histogram")
            (const "summary")


encodeMetricId :: MetricId -> Builder
encodeMetricId mid = encodeName (name mid) <> encodeLabels (labels mid)


encodeName :: Name -> Builder
encodeName = text . unName


encodeLabels :: Labels -> Builder
encodeLabels ls
    | null ls = space
    | otherwise =
        openBracket
            <> (mconcat . intersperse comma . map encodeLabel $ toList ls)
            <> closeBracket


encodeLabel :: (Text, Text) -> Builder
encodeLabel (key, val) = text key <> equals <> quote <> text (escape val) <> quote


textValue :: RealFloat f => f -> Text
textValue x
    | isInfinite x && x > 0 = "+Inf"
    | isInfinite x && x < 0 = "-Inf"
    | isNaN x = "NaN"
    | otherwise = toStrict . toLazyText $ formatRealFloat Generic Nothing x


encodeDouble :: RealFloat f => f -> Builder
encodeDouble = text . textValue


encodeInt :: Int -> Builder
encodeInt = intDec


text :: Text -> Builder
text = byteString . encodeUtf8


escape :: Text -> Text
escape = replace "\n" "\\n" . replace "\"" "\\\"" . replace "\\" "\\\\"


space :: Builder
space = char8 ' '


newline :: Builder
newline = char8 '\n'


openBracket :: Builder
openBracket = char8 '{'


closeBracket :: Builder
closeBracket = char8 '}'


comma :: Builder
comma = char8 ','


equals :: Builder
equals = char8 '='


quote :: Builder
quote = char8 '"'
