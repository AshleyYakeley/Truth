module Pinafore.Language.Type.Storable.Dynamic
    ( DynamicType
    , mkDynamicType
    , DynamicEntityType
    , DynamicEntity(..)
    , ADynamicEntityFamily(..)
    , aDynamicStorableFamilyWitness
    , dynamicStoreAdapter
    , dynamicStorableGroundType
    , aDynamicStorableGroundType
    , getConcreteDynamicEntityType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Storable.Type
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

typeStoreAdapter :: Maybe DynamicEntityType -> StoreAdapter DynamicType
typeStoreAdapter mdt = let
    storeAdapterDefinitions =
        MkEntityStorer $
        pure $
        MkKnowShim PlainConstructorStorer $ \(MkDynamicType -> t) ->
            case mdt of
                Just dt
                    | member t dt -> Known t
                Just _ -> Unknown
                Nothing -> Known t
    storeAdapterToDefinition (MkDynamicType e) = MkSomeOf (MkEntityStorer PlainConstructorStorer) e
    in MkStoreAdapter {..}

dynamicAnchor :: Anchor
dynamicAnchor = codeAnchor "pinafore-base:dynamic"

dynamicStoreAdapter :: Maybe DynamicEntityType -> StoreAdapter DynamicEntity
dynamicStoreAdapter mdt =
    invmap (\(t, (v, ())) -> MkDynamicEntity t v) (\(MkDynamicEntity t v) -> (t, (v, ()))) $
    constructorStoreAdapter
        dynamicAnchor
        (ConsListType (typeStoreAdapter mdt) $ ConsListType plainStoreAdapter NilListType)

dynamicStorableGroundType :: QGroundType '[] DynamicEntity
dynamicStorableGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily DynamicEntity)|]) "DynamicEntity")
        {qgtSubtypeGroup = Just dynamicEntitySubtypeGroup}

-- P <: DynamicEntity
dynamicTest :: QGroundType '[] DynamicEntity -> QGroundType '[] DynamicEntity -> Bool
dynamicTest ta tb =
    fromMaybe False $ do
        (Refl, HRefl) <- groundTypeTestEquality dynamicStorableGroundType tb
        Refl <- testEquality (qgtVarianceType ta) NilListType
        MkADynamicEntityFamily _ _ <- getGroundFamily aDynamicStorableFamilyWitness ta
        return True

-- P <: Q
aDynamicTest :: QGroundType '[] DynamicEntity -> QGroundType '[] DynamicEntity -> Bool
aDynamicTest ta tb =
    fromMaybe False $ do
        Refl <- testEquality (qgtVarianceType ta) NilListType
        MkADynamicEntityFamily _ deta <- getGroundFamily aDynamicStorableFamilyWitness ta
        Refl <- testEquality (qgtVarianceType tb) NilListType
        MkADynamicEntityFamily _ detb <- getGroundFamily aDynamicStorableFamilyWitness tb
        return $ isSubsetOf deta detb

dynamicEntitySubtypeGroup :: SubtypeGroup QGroundType '[] DynamicEntity
dynamicEntitySubtypeGroup =
    MkSubtypeGroup dynamicStorableGroundType $ \ta tb ->
        or [testEqualitySubtypeGroupTest ta tb, dynamicTest ta tb, aDynamicTest ta tb]

data ADynamicEntityFamily :: FamilyKind where
    MkADynamicEntityFamily :: FullName -> DynamicEntityType -> ADynamicEntityFamily DynamicEntity

instance TestHetEquality ADynamicEntityFamily where
    testHetEquality (MkADynamicEntityFamily _ dt1) (MkADynamicEntityFamily _ dt2) =
        if dt1 == dt2
            then Just HRefl
            else Nothing

aDynamicStorableFamilyWitness :: IOWitness ('MkWitKind ADynamicEntityFamily)
aDynamicStorableFamilyWitness = $(iowitness [t|'MkWitKind ADynamicEntityFamily|])

aDynamicEntityStorability :: DynamicEntityType -> Storability '[] DynamicEntity
aDynamicEntityStorability dts = let
    stbKind = NilListType
    stbCovaryMap :: CovaryMap DynamicEntity
    stbCovaryMap = covarymap
    stbAdapter :: forall ta. Arguments StoreAdapter DynamicEntity ta -> StoreAdapter ta
    stbAdapter NilArguments = dynamicStoreAdapter $ Just dts
    in MkStorability {..}

aDynamicStorableGroundType :: FullName -> DynamicEntityType -> QGroundType '[] DynamicEntity
aDynamicStorableGroundType name dts = let
    props = singleGroundProperty storabilityProperty $ aDynamicEntityStorability dts
    in (singleGroundType' (MkFamilialType aDynamicStorableFamilyWitness $ MkADynamicEntityFamily name dts) props $
        exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 SimplePolyGreatestDynamicSupertype
                     dynamicStorableGroundType
                     (functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (member dt dts) de)
                     id
           , qgtSubtypeGroup = Just dynamicEntitySubtypeGroup
           }

getConcreteDynamicEntityType :: Some (QType 'Positive) -> QInterpreter (FullName, DynamicType)
getConcreteDynamicEntityType (MkSome tm) =
    case dolanToMaybeType @QGroundType @_ @_ @(QPolyShim Type) tm of
        Just (MkShimWit (MkDolanGroundedType gt NilCCRArguments) _)
            | Just (MkADynamicEntityFamily name (toList -> [dt])) <- getGroundFamily aDynamicStorableFamilyWitness gt ->
                return (name, dt)
        _ -> throw $ InterpretTypeNotConcreteDynamicEntityError $ exprShow tm
