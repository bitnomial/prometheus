{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Sample (
    Sample (..),
    defaultSample,

    -- * SampleValue
    SampleValue,
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


defaultSample :: Sample
defaultSample =
    Sample
        { suffix = ""
        , labels = []
        , value = 0
        }


type SampleValue = Double
class ToSampleValue a where
    toSampleValue :: a -> SampleValue
instance ToSampleValue Int where
    toSampleValue = fromIntegral
instance ToSampleValue Int64 where
    toSampleValue = fromIntegral
instance ToSampleValue Word64 where
    toSampleValue = fromIntegral
instance ToSampleValue Double where
    toSampleValue = id
instance ToSampleValue Float where
    toSampleValue = realToFrac


type SampleValueNum a = (Num a, ToSampleValue a)
