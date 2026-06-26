{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Sample (
    Sample (..),
    defaultSample,

    -- * SampleValue
    SampleValue (..),
    ToSampleValue (..),
    SampleValueNum,
) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Prometheus.V3.Label (Label)


data Sample = Sample
    { suffix :: Text
    , labels :: [Label]
    , value :: SampleValue
    }


data SampleValue
    = SampleValueInt64 Int64
    | SampleValueDouble Double


defaultSample :: Sample
defaultSample =
    Sample
        { suffix = ""
        , labels = []
        , value = SampleValueInt64 0
        }


class ToSampleValue a where
    toSampleValue :: a -> SampleValue
instance ToSampleValue Int where
    toSampleValue = SampleValueInt64 . fromIntegral
instance ToSampleValue Int64 where
    toSampleValue = SampleValueInt64
instance ToSampleValue Word64 where
    toSampleValue = SampleValueInt64 . fromIntegral
instance ToSampleValue Double where
    toSampleValue = SampleValueDouble
instance ToSampleValue Float where
    toSampleValue = SampleValueDouble . realToFrac


type SampleValueNum a = (Num a, ToSampleValue a)
