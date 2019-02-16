module Pinafore.Language.OpenEntity where

import Pinafore.Base
import Shapes

newtype OpenEntity (name :: Symbol) = MkOpenEntity
    { unNamedEntity :: Entity
    } deriving (Eq)

newtype NewEntity = MkNewEntity
    { unNewEntity :: Entity
    } deriving (Eq)
