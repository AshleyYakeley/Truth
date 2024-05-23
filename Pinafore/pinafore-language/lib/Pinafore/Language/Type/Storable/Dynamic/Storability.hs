module Pinafore.Language.Type.Storable.Dynamic.Storability
    ( DynamicTypeSet
    , dynamicStoreAdapter
    , dynamicEntityStorability
    ) where

import Import
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Storable.Dynamic.Entity
import Pinafore.Language.Type.Storable.Type

type DynamicTypeSet = HashSet ConcreteDynamicType

typeStoreAdapter :: Maybe DynamicTypeSet -> StoreAdapter ConcreteDynamicType
typeStoreAdapter mdt = let
    storeAdapterDefinitions =
        MkEntityStorer $
        pure $
        MkKnowShim PlainConstructorStorer $ \(MkConcreteDynamicType -> t) ->
            case mdt of
                Just dt
                    | member t dt -> Known t
                Just _ -> Unknown
                Nothing -> Known t
    storeAdapterToDefinition (MkConcreteDynamicType e) = MkSomeOf (MkEntityStorer PlainConstructorStorer) e
    in MkStoreAdapter {..}

dynamicAnchor :: Anchor
dynamicAnchor = codeAnchor "pinafore-base:dynamic"

dynamicStoreAdapter :: Maybe DynamicTypeSet -> StoreAdapter DynamicEntity
dynamicStoreAdapter mdt =
    invmap (\(t, (v, ())) -> MkDynamicEntity t v) (\(MkDynamicEntity t v) -> (t, (v, ()))) $
    constructorStoreAdapter
        dynamicAnchor
        (ConsListType (typeStoreAdapter mdt) $ ConsListType plainStoreAdapter NilListType)

dynamicEntityStorability :: Interpreter (Maybe DynamicTypeSet) -> Storability '[] DynamicEntity
dynamicEntityStorability imdts = let
    stbKind = NilListType
    stbCovaryMap :: CovaryMap DynamicEntity
    stbCovaryMap = covarymap
    stbAdapter = do
        mdts <- imdts
        return $ MkAllFor $ \NilArguments -> dynamicStoreAdapter mdts
    in MkStorability {..}
