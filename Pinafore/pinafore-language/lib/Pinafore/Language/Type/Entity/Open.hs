module Pinafore.Language.Type.Entity.Open where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Type
import Shapes

type OpenEntityType :: BigNat -> Type
data OpenEntityType tid =
    MkOpenEntityType Name
                     (TypeIDType tid)

instance TestEquality OpenEntityType where
    testEquality (MkOpenEntityType _ t1) (MkOpenEntityType _ t2) = do
        Refl <- testEquality t1 t2
        return Refl

instance ExprShow (OpenEntityType tid) where
    exprShowPrec (MkOpenEntityType n _) = exprShowPrec n

type OpenEntity :: BigNat -> Type
newtype OpenEntity tid = MkOpenEntity
    { unOpenEntity :: Entity
    } deriving (Eq, Random)

openEntityFamilyWitness :: IOWitness ('MkWitKind (LiftedFamily OpenEntityType OpenEntity))
openEntityFamilyWitness = $(iowitness [t|'MkWitKind (LiftedFamily OpenEntityType OpenEntity)|])

openEntityGroundType :: forall tid. OpenEntityType tid -> PinaforeGroundType '[] (OpenEntity tid)
openEntityGroundType oet =
    singleGroundType' (MkFamilyType openEntityFamilyWitness $ MkLiftedFamily oet) $ exprShowPrec oet

openEntityFamily :: EntityFamily
openEntityFamily =
    MkEntityFamily openEntityFamilyWitness $ \NilListType (MkLiftedFamily oet :: _ t) -> let
        epCovaryMap = covarymap
        epEq :: forall (ta :: Type). Arguments (MonoType EntityGroundType) t ta -> Dict (Eq ta)
        epEq NilArguments = Dict
        epAdapter :: forall ta. Arguments MonoEntityType t ta -> EntityAdapter ta
        epAdapter NilArguments = isoMap MkOpenEntity unOpenEntity plainEntityAdapter
        epShowType = exprShowPrec oet
        in Just MkEntityProperties {..}

getOpenEntityType :: MonadThrow ErrorType m => AnyW (PinaforeType 'Positive) -> m (AnyW OpenEntityType)
getOpenEntityType (MkAnyW tm) =
    case dolanTypeToSingular tm of
        Just (MkAnyW (GroundedDolanSingularType gt NilDolanArguments))
            | Just (MkLiftedFamily t) <- matchFamilyType openEntityFamilyWitness $ pgtFamilyType gt -> return $ MkAnyW t
        _ -> throw $ InterpretTypeNotOpenEntityError $ exprShow tm
