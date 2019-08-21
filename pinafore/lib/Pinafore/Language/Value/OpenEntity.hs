module Pinafore.Language.Value.OpenEntity where

import Pinafore.Base
import Shapes

newtype OpenEntity (tid :: BigNat) = MkOpenEntity
    { unNamedEntity :: Entity
    } deriving (Eq)

newtype NewEntity = MkNewEntity
    { unNewEntity :: Entity
    } deriving (Eq)
