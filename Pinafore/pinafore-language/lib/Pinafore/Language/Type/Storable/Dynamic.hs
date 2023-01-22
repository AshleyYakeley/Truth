module Pinafore.Language.Type.Storable.Dynamic
    ( DynamicType
    , mkDynamicType
    , DynamicEntityType
    , DynamicEntity(..)
    , dynamicStoreAdapter
    , dynamicStorableGroundType
    , dynamicStorableFamily
    , ADynamicStorableFamily(..)
    , aDynamicStorableFamilyWitness
    , aDynamicStorableGroundType
    , aDynamicEntityStorableFamily
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
import Pinafore.Language.Type.Type
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
dynamicTest :: SubtypeGroupTest QGroundType
dynamicTest =
    MkSubtypeGroupTest $ \ta tb -> do
        (Refl, HRefl) <- groundTypeTestEquality dynamicStorableGroundType tb
        Refl <- testEquality (qgtVarianceType ta) NilListType
        MkADynamicStorableFamily _ _ <- matchFamilyType aDynamicStorableFamilyWitness $ qgtFamilyType ta
    --Refl <- testEquality (qgtVarianceType tb) NilListType
    --MkADynamicStorableFamily _ detb <- matchFamilyType aDynamicStorableFamilyWitness $ qgtFamilyType tb
        return identitySubtypeConversion

-- P <: Q
aDynamicTest :: SubtypeGroupTest QGroundType
aDynamicTest =
    MkSubtypeGroupTest $ \ta tb -> do
        Refl <- testEquality (qgtVarianceType ta) NilListType
        MkADynamicStorableFamily _ deta <- matchFamilyType aDynamicStorableFamilyWitness $ qgtFamilyType ta
        Refl <- testEquality (qgtVarianceType tb) NilListType
        MkADynamicStorableFamily _ detb <- matchFamilyType aDynamicStorableFamilyWitness $ qgtFamilyType tb
        ifpure (isSubsetOf deta detb) identitySubtypeConversion

dynamicEntitySubtypeGroup :: SubtypeGroup QGroundType
dynamicEntitySubtypeGroup =
    MkSubtypeGroup (MkSomeGroundType dynamicStorableGroundType) $
    testEqualitySubtypeGroupTest <> dynamicTest <> aDynamicTest

dynamicStorableFamily :: StorableFamily
dynamicStorableFamily = simplePinaforeStorableFamily dynamicStorableGroundType $ dynamicStoreAdapter Nothing

data ADynamicStorableFamily :: FamilyKind where
    MkADynamicStorableFamily :: FullName -> DynamicEntityType -> ADynamicStorableFamily DynamicEntity

instance TestHetEquality ADynamicStorableFamily where
    testHetEquality (MkADynamicStorableFamily _ dt1) (MkADynamicStorableFamily _ dt2) =
        if dt1 == dt2
            then Just HRefl
            else Nothing

aDynamicStorableFamilyWitness :: IOWitness ('MkWitKind ADynamicStorableFamily)
aDynamicStorableFamilyWitness = $(iowitness [t|'MkWitKind ADynamicStorableFamily|])

aDynamicStorableGroundType :: FullName -> DynamicEntityType -> QGroundType '[] DynamicEntity
aDynamicStorableGroundType name dts =
    (singleGroundType' (MkFamilialType aDynamicStorableFamilyWitness $ MkADynamicStorableFamily name dts) $
     exprShowPrec name)
        { qgtGreatestDynamicSupertype =
              SimplePolyGreatestDynamicSupertype
                  dynamicStorableGroundType
                  (functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (member dt dts) de)
                  id
        , qgtSubtypeGroup = Just dynamicEntitySubtypeGroup
        }

aDynamicEntityStorableFamily :: StorableFamily
aDynamicEntityStorableFamily =
    MkStorableFamily aDynamicStorableFamilyWitness $ \(MkADynamicStorableFamily name dt) -> let
        stbKind = NilListType
        stbCovaryMap :: CovaryMap DynamicEntity
        stbCovaryMap = covarymap
        stbAdapter :: forall ta. Arguments StoreAdapter DynamicEntity ta -> StoreAdapter ta
        stbAdapter NilArguments = dynamicStoreAdapter $ Just dt
        stbShowType = exprShowPrec name
        in Just $ MkSealedStorability MkStorability {..}

getConcreteDynamicEntityType :: Some (QType 'Positive) -> QInterpreter (FullName, DynamicType)
getConcreteDynamicEntityType (MkSome tm) =
    case dolanToMaybeType @QGroundType @_ @_ @(QPolyShim Type) tm of
        Just (MkShimWit (MkDolanGroundedType gt NilCCRArguments) _)
            | Just (MkADynamicStorableFamily n (toList -> [dt])) <-
                 matchFamilyType aDynamicStorableFamilyWitness $ qgtFamilyType gt -> return (n, dt)
        _ -> throwWithName $ \ntt -> InterpretTypeNotConcreteDynamicEntityError $ ntt $ exprShow tm
