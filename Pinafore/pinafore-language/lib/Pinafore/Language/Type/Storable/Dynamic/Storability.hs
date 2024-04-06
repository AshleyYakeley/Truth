module Pinafore.Language.Type.Storable.Dynamic.Storability
    ( DynamicTypeSet
    , dynamicStoreAdapter
    , dynamicEntityStorability
    ) where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Type.Storable.Dynamic.Entity
import Pinafore.Language.Type.Storable.Type
import Shapes

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

dynamicEntityStorability :: Maybe DynamicTypeSet -> Storability '[] DynamicEntity
dynamicEntityStorability mdts = let
    stbKind = NilListType
    stbCovaryMap :: CovaryMap DynamicEntity
    stbCovaryMap = covarymap
    stbAdapter :: forall ta. Arguments StoreAdapter DynamicEntity ta -> StoreAdapter ta
    stbAdapter NilArguments = dynamicStoreAdapter mdts
    in MkStorability {..}
