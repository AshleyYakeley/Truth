module Pinafore.Base.Entity where

import Pinafore.Base.Point
import Shapes

newtype Entity =
    MkEntity Point
    deriving (Eq, Serialize)

pointToEntity :: Point -> Entity
pointToEntity = MkEntity

pairToEntity :: (Entity, Entity) -> Entity
pairToEntity (a, b) = pointToEntity $ hashToPoint $ \call -> [call @Text "pair:", call a, call b]

eitherToEntity :: Either Entity Entity -> Entity
eitherToEntity (Left v) = pointToEntity $ hashToPoint $ \call -> [call @Text "Left:", call v]
eitherToEntity (Right v) = pointToEntity $ hashToPoint $ \call -> [call @Text "Right:", call v]
