module Pinafore.Language.Type.Storable.Open
    ( OpenEntityType(..)
    , OpenEntity(..)
    , OpenEntityFamily(..)
    , openStorableFamilyWitness
    , openStorableGroundType
    , getOpenEntityType
    ) where

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

data OpenEntityFamily :: FamilyKind where
    MkOpenEntityFamily :: OpenEntityType tid -> OpenEntityFamily (OpenEntity tid)

instance TestHetEquality OpenEntityFamily where
    testHetEquality (MkOpenEntityFamily t1) (MkOpenEntityFamily t2) = do
        Refl <- testEquality t1 t2
        return HRefl

openStorableFamilyWitness :: IOWitness ('MkWitKind OpenEntityFamily)
openStorableFamilyWitness = $(iowitness [t|'MkWitKind OpenEntityFamily|])

openStorableGroundType :: forall tid. OpenEntityType tid -> QGroundType '[] (OpenEntity tid)
openStorableGroundType oet = let
    storability :: Storability '[] (OpenEntity tid)
    storability = let
        stbKind = NilListType
        stbCovaryMap = covarymap
        stbAdapter :: forall ta. Arguments StoreAdapter (OpenEntity tid) ta -> StoreAdapter ta
        stbAdapter NilArguments = invmap MkOpenEntity unOpenEntity plainStoreAdapter
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    in singleGroundType' (MkFamilialType openStorableFamilyWitness $ MkOpenEntityFamily oet) props $ exprShowPrec oet

getOpenEntityType :: Some (QType 'Positive) -> QInterpreter (Some OpenEntityType)
getOpenEntityType (MkSome tm) =
    case dolanToMaybeType @QGroundType @_ @_ @(QPolyShim Type) tm of
        Just (MkShimWit (MkDolanGroundedType gt NilCCRArguments) _)
            | Just (MkOpenEntityFamily oet) <- getGroundFamily openStorableFamilyWitness gt -> return $ MkSome oet
        _ -> throw $ InterpretTypeNotOpenEntityError $ exprShow tm
