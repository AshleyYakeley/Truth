module Pinafore.Language.Type.Storable.Open where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Storable.Type
import Pinafore.Language.Type.Type
import Shapes

type OpenEntityType :: Nat -> Type
data OpenEntityType tid =
    MkOpenEntityType FullName
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

openStorableFamilyWitness :: IOWitness ('MkWitKind (LiftedFamily OpenEntityType OpenEntity))
openStorableFamilyWitness = $(iowitness [t|'MkWitKind (LiftedFamily OpenEntityType OpenEntity)|])

openStorableGroundType :: forall tid. OpenEntityType tid -> QGroundType '[] (OpenEntity tid)
openStorableGroundType oet =
    singleGroundType' (MkFamilialType openStorableFamilyWitness $ MkLiftedFamily oet) $ exprShowPrec oet

openStorableFamily :: StorableFamily
openStorableFamily =
    MkStorableFamily openStorableFamilyWitness $ \(MkLiftedFamily oet :: _ t) -> let
        stbKind = NilListType
        stbCovaryMap = covarymap
        stbAdapter :: forall ta. Arguments StoreAdapter t ta -> StoreAdapter ta
        stbAdapter NilArguments = invmap MkOpenEntity unOpenEntity plainStoreAdapter
        stbShowType = exprShowPrec oet
        in Just $ MkSealedStorability MkStorability {..}

getOpenEntityType :: Some (QType 'Positive) -> QInterpreter (Some OpenEntityType)
getOpenEntityType (MkSome tm) =
    case dolanToMaybeType @QGroundType @_ @_ @(QPolyShim Type) tm of
        Just (MkShimWit (MkDolanGroundedType gt NilCCRArguments) _)
            | Just (MkLiftedFamily t) <- matchFamilyType openStorableFamilyWitness $ qgtFamilyType gt ->
                return $ MkSome t
        _ -> throwWithName $ \ntt -> InterpretTypeNotOpenEntityError $ ntt $ exprShow tm
