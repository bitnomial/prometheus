module Prometheus.V3.Utils.FromRealFrac (
    FromRealFrac (..),
) where

import Data.Word (Word64)


class (Num a) => FromRealFrac a where
    fromRealFrac :: (RealFrac x) => x -> a


instance FromRealFrac Int where
    fromRealFrac = round
instance FromRealFrac Word64 where
    fromRealFrac = round


instance FromRealFrac Double where
    fromRealFrac = realToFrac
instance FromRealFrac Float where
    fromRealFrac = realToFrac
