{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Prometheus.V3.MetricName (
    MetricName,
    fromTextUnsafe,
    fromText,
    toText,
    toName,
) where

import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Prometheus.V3.Name (IsName (..), Name, makeName)
import qualified Prometheus.V3.Name as Name


-- | A Name can be any UTF-8 string, although it's recommended to match the
--   regex @[a-zA-Z_:][a-zA-Z0-9_:]*@.
--
-- https://prometheus.io/docs/concepts/data_model/
newtype MetricName = MetricName Name
    deriving stock (Show)
    deriving newtype (Eq, Ord)


instance IsString MetricName where
    fromString = fromTextUnsafe . Text.pack
instance IsName MetricName where
    getName (MetricName name) = name
    isValidLegacyChar _ c =
        or
            [ 'a' <= c && c <= 'z'
            , 'A' <= c && c <= 'Z'
            , '0' <= c && c <= '9'
            , c == '_'
            , c == ':'
            ]


fromTextUnsafe :: (HasCallStack) => Text -> MetricName
fromTextUnsafe = either (error . Text.unpack) id . fromText


fromText :: Text -> Either Text MetricName
fromText s
    | Text.null s = Left "Metric names must not be empty"
    | otherwise = Right . MetricName $ makeName (Proxy @MetricName) s


toText :: MetricName -> Text
toText = Name.toText . toName


toName :: MetricName -> Name
toName (MetricName n) = n
