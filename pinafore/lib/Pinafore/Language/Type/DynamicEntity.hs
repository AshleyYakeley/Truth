module Pinafore.Language.Type.DynamicEntity
    ( DynamicType
    , mkDynamicType
    , DynamicEntityType
    , DynamicEntity
    , dynamicEntityAdapter
    ) where

import Pinafore.Base
import Shapes

newtype DynamicType =
    MkDynamicType Entity
    deriving (Eq, Hashable)

mkDynamicType :: Anchor -> DynamicType
mkDynamicType a = MkDynamicType $ MkEntity a

type DynamicEntityType = HashSet DynamicType

data DynamicEntity =
    MkDynamicEntity DynamicType
                    Entity
    deriving (Eq)

typeEntityAdapter :: DynamicEntityType -> EntityAdapter DynamicType
typeEntityAdapter dt = let
    entityAdapterDefinitions =
        MkEntityStorer $
        pure $
        MkKnowShim PlainConstructorStorer $ \(MkDynamicType -> t) ->
            if member t dt
                then Known t
                else Unknown
    entityAdapterToDefinition (MkDynamicType e) = MkAnyValue (MkEntityStorer PlainConstructorStorer) e
    in MkEntityAdapter {..}

dynamicAnchor :: Anchor
dynamicAnchor = codeAnchor "pinafore-base:dynamic"

dynamicEntityAdapter :: DynamicEntityType -> EntityAdapter DynamicEntity
dynamicEntityAdapter dt =
    isoMap (\(t, (v, ())) -> MkDynamicEntity t v) (\(MkDynamicEntity t v) -> (t, (v, ()))) $
    constructorEntityAdapter
        dynamicAnchor
        (ConsListType (typeEntityAdapter dt) $ ConsListType plainEntityAdapter NilListType)
