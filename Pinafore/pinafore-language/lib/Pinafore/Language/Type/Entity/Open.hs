module Pinafore.Language.Type.Entity.Open where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Type
import Shapes

type OpenEntityType :: Nat -> Type
data OpenEntityType tid =
    MkOpenEntityType Name
                     (TypeIDType tid)

instance TestEquality OpenEntityType where
    testEquality (MkOpenEntityType _ t1) (MkOpenEntityType _ t2) = do
        Refl <- testEquality t1 t2
        return Refl

instance ExprShow (OpenEntityType tid) where
    exprShowPrec (MkOpenEntityType n _) = exprShowPrec n

type OpenEntity :: Nat -> Type
newtype OpenEntity tid = MkOpenEntity
    { unOpenEntity :: Entity
    } deriving (Eq, Random)

openEntityFamilyWitness :: IOWitness ('MkWitKind (LiftedFamily OpenEntityType OpenEntity))
openEntityFamilyWitness = $(iowitness [t|'MkWitKind (LiftedFamily OpenEntityType OpenEntity)|])

openEntityGroundType :: forall tid. OpenEntityType tid -> PinaforeGroundType '[] (OpenEntity tid)
openEntityGroundType oet =
    singleGroundType' (MkFamilialType openEntityFamilyWitness $ MkLiftedFamily oet) $ exprShowPrec oet

openEntityFamily :: EntityFamily
openEntityFamily =
    MkEntityFamily openEntityFamilyWitness $ \(MkLiftedFamily oet :: _ t) -> let
        epKind = NilListType
        epCovaryMap = covarymap
        epAdapter :: forall ta. Arguments EntityAdapter t ta -> EntityAdapter ta
        epAdapter NilArguments = invmap MkOpenEntity unOpenEntity plainEntityAdapter
        epShowType = exprShowPrec oet
        in Just $ MkSealedEntityProperties MkEntityProperties {..}

getOpenEntityType :: MonadThrow ErrorType m => Some (PinaforeType 'Positive) -> m (Some OpenEntityType)
getOpenEntityType (MkSome tm) =
    case dolanTypeToSingular @PinaforeGroundType @(PinaforePolyShim Type) tm of
        Just (MkShimWit (GroundedDolanSingularType gt NilCCRArguments) _)
            | Just (MkLiftedFamily t) <- matchFamilyType openEntityFamilyWitness $ pgtFamilyType gt -> return $ MkSome t
        _ -> throw $ InterpretTypeNotOpenEntityError $ exprShow tm
