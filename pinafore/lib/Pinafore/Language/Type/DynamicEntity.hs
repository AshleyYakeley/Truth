module Pinafore.Language.Type.DynamicEntity
    ( DynamicType
    , mkDynamicType
    , DynamicEntityType
    , DynamicEntity(..)
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

typeEntityAdapter :: Maybe DynamicEntityType -> EntityAdapter DynamicType
typeEntityAdapter mdt = let
    entityAdapterDefinitions =
        MkEntityStorer $
        pure $
        MkKnowShim PlainConstructorStorer $ \(MkDynamicType -> t) ->
            case mdt of
                Just dt
                    | member t dt -> Known t
                Just _ -> Unknown
                Nothing -> Known t
    entityAdapterToDefinition (MkDynamicType e) = MkAnyValue (MkEntityStorer PlainConstructorStorer) e
    in MkEntityAdapter {..}

dynamicAnchor :: Anchor
dynamicAnchor = codeAnchor "pinafore-base:dynamic"

dynamicEntityAdapter :: Maybe DynamicEntityType -> EntityAdapter DynamicEntity
dynamicEntityAdapter mdt =
    isoMap (\(t, (v, ())) -> MkDynamicEntity t v) (\(MkDynamicEntity t v) -> (t, (v, ()))) $
    constructorEntityAdapter
        dynamicAnchor
        (ConsListType (typeEntityAdapter mdt) $ ConsListType plainEntityAdapter NilListType)
