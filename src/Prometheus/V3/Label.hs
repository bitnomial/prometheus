{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Prometheus.V3.Label (
    LabelName,
    LabelValue,
    Label,
    IsLabelValue (..),
    IsLabelNameTuple (..),
    IsLabelValueTuple (..),
) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (Solo (..))
import Prometheus.V3.Encode.Value (showPromDouble)
import Prometheus.V3.LabelName (LabelName)


type LabelValue = Text


type Label = (LabelName, LabelValue)


-- | A class for all types that can be used as the value of a Label.
class (Hashable a) => IsLabelValue a where
    toLabelValue :: a -> LabelValue


instance IsLabelValue Text where
    toLabelValue = id
instance IsLabelValue Int where
    toLabelValue = Text.pack . show
instance IsLabelValue Double where
    toLabelValue = showPromDouble


class IsLabelNameTuple names where
    toLabelNameList :: names -> [LabelName]


class
    ( IsLabelNameTuple (LabelTupleNames values)
    , Hashable values
    ) =>
    IsLabelValueTuple values
    where
    type LabelTupleNames values
    toLabelValueList :: values -> [LabelValue]


instance IsLabelNameTuple LabelName where
    toLabelNameList a = [a]
instance (IsLabelValue a) => IsLabelValueTuple (Solo a) where
    type LabelTupleNames (Solo a) = LabelName
    toLabelValueList (MkSolo a) = [toLabelValue a]
instance
    IsLabelNameTuple
        ( LabelName
        , LabelName
        )
    where
    toLabelNameList (a, b) = [a, b]
instance
    ( IsLabelValue a
    , IsLabelValue b
    ) =>
    IsLabelValueTuple (a, b)
    where
    type LabelTupleNames (a, b) = (LabelName, LabelName)
    toLabelValueList (a, b) = [toLabelValue a, toLabelValue b]
instance
    IsLabelNameTuple
        ( LabelName
        , LabelName
        , LabelName
        )
    where
    toLabelNameList (a, b, c) = [a, b, c]
instance
    ( IsLabelValue a
    , IsLabelValue b
    , IsLabelValue c
    ) =>
    IsLabelValueTuple (a, b, c)
    where
    type LabelTupleNames (a, b, c) = (LabelName, LabelName, LabelName)
    toLabelValueList (a, b, c) =
        [ toLabelValue a
        , toLabelValue b
        , toLabelValue c
        ]
instance
    IsLabelNameTuple
        ( LabelName
        , LabelName
        , LabelName
        , LabelName
        )
    where
    toLabelNameList (a, b, c, d) = [a, b, c, d]
instance
    ( IsLabelValue a
    , IsLabelValue b
    , IsLabelValue c
    , IsLabelValue d
    ) =>
    IsLabelValueTuple (a, b, c, d)
    where
    type LabelTupleNames (a, b, c, d) = (LabelName, LabelName, LabelName, LabelName)
    toLabelValueList (a, b, c, d) =
        [ toLabelValue a
        , toLabelValue b
        , toLabelValue c
        , toLabelValue d
        ]
instance
    IsLabelNameTuple
        ( LabelName
        , LabelName
        , LabelName
        , LabelName
        , LabelName
        )
    where
    toLabelNameList (a, b, c, d, e) = [a, b, c, d, e]
instance
    ( IsLabelValue a
    , IsLabelValue b
    , IsLabelValue c
    , IsLabelValue d
    , IsLabelValue e
    ) =>
    IsLabelValueTuple (a, b, c, d, e)
    where
    type LabelTupleNames (a, b, c, d, e) = (LabelName, LabelName, LabelName, LabelName, LabelName)
    toLabelValueList (a, b, c, d, e) =
        [ toLabelValue a
        , toLabelValue b
        , toLabelValue c
        , toLabelValue d
        , toLabelValue e
        ]
instance
    IsLabelNameTuple
        ( LabelName
        , LabelName
        , LabelName
        , LabelName
        , LabelName
        , LabelName
        )
    where
    toLabelNameList (a, b, c, d, e, f) = [a, b, c, d, e, f]
instance
    ( IsLabelValue a
    , IsLabelValue b
    , IsLabelValue c
    , IsLabelValue d
    , IsLabelValue e
    , IsLabelValue f
    ) =>
    IsLabelValueTuple (a, b, c, d, e, f)
    where
    type LabelTupleNames (a, b, c, d, e, f) = (LabelName, LabelName, LabelName, LabelName, LabelName, LabelName)
    toLabelValueList (a, b, c, d, e, f) =
        [ toLabelValue a
        , toLabelValue b
        , toLabelValue c
        , toLabelValue d
        , toLabelValue e
        , toLabelValue f
        ]
instance
    IsLabelNameTuple
        ( LabelName
        , LabelName
        , LabelName
        , LabelName
        , LabelName
        , LabelName
        , LabelName
        )
    where
    toLabelNameList (a, b, c, d, e, f, g) = [a, b, c, d, e, f, g]
instance
    ( IsLabelValue a
    , IsLabelValue b
    , IsLabelValue c
    , IsLabelValue d
    , IsLabelValue e
    , IsLabelValue f
    , IsLabelValue g
    ) =>
    IsLabelValueTuple (a, b, c, d, e, f, g)
    where
    type LabelTupleNames (a, b, c, d, e, f, g) = (LabelName, LabelName, LabelName, LabelName, LabelName, LabelName, LabelName)
    toLabelValueList (a, b, c, d, e, f, g) =
        [ toLabelValue a
        , toLabelValue b
        , toLabelValue c
        , toLabelValue d
        , toLabelValue e
        , toLabelValue f
        , toLabelValue g
        ]
