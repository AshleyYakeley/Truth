module Pinafore.Language.Type.Storable.Dynamic.Entity
    ( ConcreteDynamicType(..)
    , mkConcreteDynamicType
    , DynamicEntity(..)
    ) where

import Import

newtype ConcreteDynamicType =
    MkConcreteDynamicType Entity
    deriving (Eq, Hashable)

mkConcreteDynamicType :: Anchor -> ConcreteDynamicType
mkConcreteDynamicType a = MkConcreteDynamicType $ MkEntity a

data DynamicEntity =
    MkDynamicEntity ConcreteDynamicType
                    Entity
    deriving (Eq)
