module Pinafore.Language.NamedEntity where

import Pinafore.Base
import Shapes

newtype NamedEntity (name :: Symbol) = MkNamedEntity
    { unNamedEntity :: Point
    } deriving (Eq)

namedToEntity :: NamedEntity name -> Entity
namedToEntity (MkNamedEntity p) = MkEntity p
