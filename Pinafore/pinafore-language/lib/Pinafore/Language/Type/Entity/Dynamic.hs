module Pinafore.Language.Type.Entity.Dynamic
    ( DynamicType
    , mkDynamicType
    , DynamicEntityType
    , DynamicEntity(..)
    , dynamicEntityAdapter
    , dynamicEntityGroundType
    , dynamicEntityFamily
    , ADynamicEntityFamily(..)
    , aDynamicEntityFamilyWitness
    , aDynamicEntityGroundType
    , aDynamicEntityEntityFamily
    , getConcreteDynamicEntityType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
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

dynamicEntityGroundType :: PinaforeGroundType '[] DynamicEntity
dynamicEntityGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily DynamicEntity)|]) "DynamicEntity"

dynamicEntityFamily :: EntityFamily
dynamicEntityFamily = simplePinaforeEntityFamily dynamicEntityGroundType $ dynamicEntityAdapter Nothing

data ADynamicEntityFamily :: FamilyKind where
    MkADynamicEntityFamily :: Name -> DynamicEntityType -> ADynamicEntityFamily DynamicEntity

instance TestHetEquality ADynamicEntityFamily where
    testHetEquality (MkADynamicEntityFamily _ dt1) (MkADynamicEntityFamily _ dt2) =
        if dt1 == dt2
            then Just HRefl
            else Nothing

aDynamicEntityFamilyWitness :: IOWitness ('MkWitKind ADynamicEntityFamily)
aDynamicEntityFamilyWitness = $(iowitness [t|'MkWitKind ADynamicEntityFamily|])

aDynamicEntityGroundType :: Name -> DynamicEntityType -> PinaforeGroundType '[] DynamicEntity
aDynamicEntityGroundType name dts =
    (singleGroundType' (MkFamilyType aDynamicEntityFamilyWitness $ MkADynamicEntityFamily name dts) $ exprShowPrec name)
        { pgtGreatestDynamicSupertype =
              \NilDolanArguments ->
                  Just $
                  makeNilGDS dynamicEntityGroundType $
                  functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (member dt dts) de
        }

aDynamicEntityEntityFamily :: EntityFamily
aDynamicEntityEntityFamily =
    MkEntityFamily aDynamicEntityFamilyWitness $ \NilListType (MkADynamicEntityFamily name dt) -> let
        epCovaryMap :: CovaryMap DynamicEntity
        epCovaryMap = covarymap
        epEq :: forall (ta :: Type). Arguments (MonoType EntityGroundType) DynamicEntity ta -> Dict (Eq ta)
        epEq NilArguments = Dict
        epAdapter :: forall ta. Arguments MonoEntityType DynamicEntity ta -> EntityAdapter ta
        epAdapter NilArguments = dynamicEntityAdapter $ Just dt
        epShowType = exprShowPrec name
        in Just $ MkEntityProperties {..}

getConcreteDynamicEntityType :: MonadThrow ErrorType m => AnyW (PinaforeType 'Positive) -> m (Name, DynamicType)
getConcreteDynamicEntityType (MkAnyW tm) =
    case dolanTypeToSingular tm of
        Just (MkAnyW (GroundedDolanSingularType gt NilDolanArguments))
            | Just (MkADynamicEntityFamily n (toList -> [dt])) <-
                 matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType gt -> return (n, dt)
        _ -> throw $ InterpretTypeNotConcreteDynamicEntityError $ exprShow tm
