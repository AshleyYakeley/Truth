module Pinafore.Language.Type.Storable.Dynamic.Concrete
    ( ConcreteDynamicEntityFamily(..)
    , concreteDynamicStorableFamilyWitness
    , concreteDynamicStorableGroundType
    , getConcreteDynamicEntityType
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Storable.Dynamic.DynamicEntity
import Pinafore.Language.Type.Storable.Dynamic.Entity
import Pinafore.Language.Type.Storable.Dynamic.Storability
import Pinafore.Language.Type.Storable.Type
import Shapes

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
    props = singleGroundProperty storabilityProperty $ dynamicEntityStorability $ return $ Just $ opoint cdt
    in (singleGroundType'
            (MkFamilialType concreteDynamicStorableFamilyWitness $ MkConcreteDynamicEntityFamily name cdt)
            props $
        exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 simplePolyGreatestDynamicSupertype
                     dynamicEntityStorableGroundType
                     (functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (dt == cdt) de)
           }

getConcreteDynamicEntityType :: Some (QType 'Positive) -> QInterpreter (FullName, ConcreteDynamicType)
getConcreteDynamicEntityType (MkSome tm) =
    case dolanToMaybeType @QGroundType @_ @_ @(QPolyShim Type) tm of
        Just (MkShimWit (MkDolanGroundedType gt NilCCRArguments) _)
            | Just (MkConcreteDynamicEntityFamily name cdt) <- getGroundFamily concreteDynamicStorableFamilyWitness gt ->
                return (name, cdt)
        _ -> throw $ InterpretTypeNotConcreteDynamicEntityError $ exprShow tm
