module Pinafore.Language.NamedEntity where

import Pinafore.Base
import Shapes

newtype NamedEntity (name :: Symbol) = MkNamedEntity
    { unNamedEntity :: Entity
    } deriving (Eq)

newtype NewEntity = MkNewEntity
    { unNewEntity :: Entity
    } deriving (Eq)

newNamedEntity :: NewEntity -> NamedEntity name
newNamedEntity (MkNewEntity e) = MkNamedEntity e
