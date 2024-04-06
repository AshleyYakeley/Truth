module Pinafore.Language.Type.Storable.Dynamic
    ( ConcreteDynamicType
    , mkConcreteDynamicType
    , DynamicTypeSet
    , DynamicEntity(..)
    , AbstractDynamicEntityFamily(..)
    , ConcreteDynamicEntityFamily(..)
    , dynamicStoreAdapter
    , dynamicStorableGroundType
    , abstractDynamicStorableGroundType
    , concreteDynamicStorableGroundType
    , getConcreteDynamicEntityType
    , abstractDynamicStorableFamilyWitness
    , concreteDynamicStorableFamilyWitness
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Storable.Type
import Shapes

type DynamicTypeSet = HashSet ConcreteDynamicType

newtype ConcreteDynamicType =
    MkConcreteDynamicType Entity
    deriving (Eq, Hashable)

mkConcreteDynamicType :: Anchor -> ConcreteDynamicType
mkConcreteDynamicType a = MkConcreteDynamicType $ MkEntity a

data DynamicEntity =
    MkDynamicEntity ConcreteDynamicType
                    Entity
    deriving (Eq)

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

dynamicStorableGroundType :: QGroundType '[] DynamicEntity
dynamicStorableGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily DynamicEntity)|]) "DynamicEntity")
        {qgtProperties = singleGroundProperty storabilityProperty $ dynamicEntityStorability Nothing}

data ConcreteDynamicEntityFamily :: FamilyKind where
    MkConcreteDynamicEntityFamily :: FullName -> ConcreteDynamicType -> ConcreteDynamicEntityFamily DynamicEntity

instance TestHetEquality ConcreteDynamicEntityFamily where
    testHetEquality (MkConcreteDynamicEntityFamily _ cdt1) (MkConcreteDynamicEntityFamily _ cdt2) =
        if cdt1 == cdt2
            then Just HRefl
            else Nothing

concreteDynamicStorableFamilyWitness :: IOWitness ('MkWitKind ConcreteDynamicEntityFamily)
concreteDynamicStorableFamilyWitness = $(iowitness [t|'MkWitKind ConcreteDynamicEntityFamily|])

concreteDynamicStorableGroundType :: FullName -> ConcreteDynamicType -> QGroundType '[] DynamicEntity
concreteDynamicStorableGroundType name cdt = let
    props = singleGroundProperty storabilityProperty $ dynamicEntityStorability $ Just $ opoint cdt
    in (singleGroundType'
            (MkFamilialType concreteDynamicStorableFamilyWitness $ MkConcreteDynamicEntityFamily name cdt)
            props $
        exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 SimplePolyGreatestDynamicSupertype
                     dynamicStorableGroundType
                     (functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (dt == cdt) de)
                     id
           }

data AbstractDynamicEntityFamily :: FamilyKind where
    MkAbstractDynamicEntityFamily :: TypeIDType tid -> DynamicTypeSet -> AbstractDynamicEntityFamily DynamicEntity

instance TestHetEquality AbstractDynamicEntityFamily where
    testHetEquality (MkAbstractDynamicEntityFamily t1 _) (MkAbstractDynamicEntityFamily t2 _) = do
        Refl <- testEquality t1 t2
        return HRefl

abstractDynamicStorableFamilyWitness :: IOWitness ('MkWitKind AbstractDynamicEntityFamily)
abstractDynamicStorableFamilyWitness = $(iowitness [t|'MkWitKind AbstractDynamicEntityFamily|])

abstractDynamicStorableGroundType :: FullName -> TypeIDType tid -> DynamicTypeSet -> QGroundType '[] DynamicEntity
abstractDynamicStorableGroundType name tid dts = let
    props = singleGroundProperty storabilityProperty $ dynamicEntityStorability $ Just dts
    in (singleGroundType'
            (MkFamilialType abstractDynamicStorableFamilyWitness $ MkAbstractDynamicEntityFamily tid dts)
            props $
        exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 SimplePolyGreatestDynamicSupertype
                     dynamicStorableGroundType
                     (functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (member dt dts) de)
                     id
           }

getConcreteDynamicEntityType :: Some (QType 'Positive) -> QInterpreter (FullName, ConcreteDynamicType)
getConcreteDynamicEntityType (MkSome tm) =
    case dolanToMaybeType @QGroundType @_ @_ @(QPolyShim Type) tm of
        Just (MkShimWit (MkDolanGroundedType gt NilCCRArguments) _)
            | Just (MkConcreteDynamicEntityFamily name cdt) <- getGroundFamily concreteDynamicStorableFamilyWitness gt ->
                return (name, cdt)
        _ -> throw $ InterpretTypeNotConcreteDynamicEntityError $ exprShow tm
