{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Prometheus.V3.LabelName (
    LabelName,
    fromTextUnsafe,
    fromText,
) where

import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Prometheus.V3.Name (IsName (..), Name, makeName)


-- | A LabelName can be any UTF-8 string, although it's recommended to match
--   the regex @[a-zA-Z_][a-zA-Z0-9_]*@. It must not start with "__", which will
--   be a runtime error.
--
-- https://prometheus.io/docs/concepts/data_model/
newtype LabelName = LabelName Name
    deriving stock (Show)
    deriving newtype (Eq, Ord)


instance IsString LabelName where
    fromString = fromTextUnsafe . Text.pack
instance IsName LabelName where
    getName (LabelName name) = name
    isValidLegacyChar _ c =
        or
            [ 'a' <= c && c <= 'z'
            , 'A' <= c && c <= 'Z'
            , '0' <= c && c <= '9'
            , c == '_'
            ]


fromTextUnsafe :: (HasCallStack) => Text -> LabelName
fromTextUnsafe = either (error . Text.unpack) id . fromText


fromText :: Text -> Either Text LabelName
fromText s
    | Text.null s = Left "Label names must not be empty"
    | "__" `Text.isPrefixOf` s = Left $ "Label names must not start with '__', got: " <> s
    | otherwise = Right . LabelName $ makeName (Proxy @LabelName) s
