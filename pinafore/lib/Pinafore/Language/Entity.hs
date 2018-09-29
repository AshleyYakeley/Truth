module Pinafore.Language.Entity where

import Pinafore.Table (Point)
import Shapes

newtype Entity =
    MkEntity Point
    deriving (Eq)

pointToEntity :: Point -> Entity
pointToEntity = MkEntity

newtype NamedEntity (name :: Symbol) = MkNamedEntity
    { unNamedEntity :: Point
    }

namedToEntity :: NamedEntity name -> Entity
namedToEntity (MkNamedEntity p) = MkEntity p
