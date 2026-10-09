{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.MetricId where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (null)


-- | Construct with 'makeName' to ensure that names use only valid characters
newtype Name = Name {unName :: Text} deriving (Show, Eq, Ord, Monoid, Semigroup)


instance IsString Name where
    fromString = makeName . Text.pack


newtype Labels = Labels {unLabels :: Map Text Text} deriving (Show, Eq, Ord, Monoid, Semigroup)


data MetricId = MetricId
    { name :: Name
    , labels :: Labels
    }
    deriving (Eq, Ord, Show)


addLabel :: Text -> Text -> Labels -> Labels
addLabel key val = Labels . Map.insert (makeValid key) val . unLabels


fromList :: [(Text, Text)] -> Labels
fromList = Labels . Map.fromList . map (first makeValid)


toList :: Labels -> [(Text, Text)]
toList = Map.toList . unLabels


null :: Labels -> Bool
null = Map.null . unLabels


-- | Make the input match the regex @[a-zA-Z_][a-zA-Z0-9_]@ which
-- defines valid metric and label names, according to
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>
-- Replace invalid characters with @_@ and add a leading @_@ if the
-- first character is only valid as a later character.
makeValid :: Text -> Text
makeValid "" = "_"
makeValid txt = prefix_ <> Text.map (\c -> if allowedChar c then c else '_') txt
  where
    prefix_ = if isDigit (Text.head txt) then "_" else ""
    allowedChar :: Char -> Bool
    allowedChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDigit c || c == '_'


-- | Construct a 'Name', replacing disallowed characters.
makeName :: Text -> Name
makeName = Name . makeValid
