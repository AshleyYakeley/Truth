module Pinafore.Language.Entity where

import Pinafore.Point
import Shapes

newtype Entity =
    MkEntity Point
    deriving (Eq, Serialize)

pointToEntity :: Point -> Entity
pointToEntity = MkEntity

newtype NamedEntity (name :: Symbol) = MkNamedEntity
    { unNamedEntity :: Point
    }

namedToEntity :: NamedEntity name -> Entity
namedToEntity (MkNamedEntity p) = MkEntity p

pairToEntity :: (Entity, Entity) -> Entity
pairToEntity (a, b) = pointToEntity $ hashToPoint $ \call -> [call @Text "pair", call a, call b]

eitherToEntity :: Either Entity Entity -> Entity
eitherToEntity (Left v) = pointToEntity $ hashToPoint $ \call -> [call @Text "Left", call v]
eitherToEntity (Right v) = pointToEntity $ hashToPoint $ \call -> [call @Text "Right", call v]
