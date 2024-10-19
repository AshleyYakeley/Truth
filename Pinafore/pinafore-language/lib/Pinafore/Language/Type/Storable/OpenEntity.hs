module Pinafore.Language.Type.Storable.OpenEntity
    ( OpenEntityType(..)
    , OpenEntity(..)
    , OpenEntityFamily(..)
    , openEntityFamilyWitness
    , openEntityGroundType
    , getOpenEntityType
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Storable.Type

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

openEntityFamilyWitness :: IOWitness ('MkWitKind OpenEntityFamily)
openEntityFamilyWitness = $(iowitness [t|'MkWitKind OpenEntityFamily|])

openEntityGroundType :: forall tid. OpenEntityType tid -> QGroundType '[] (OpenEntity tid)
openEntityGroundType oet = let
    storability :: Storability '[] (OpenEntity tid)
    storability = let
        stbKind = NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot :: QExprKnot (WithStoreAdapterArgs (OpenEntity tid) StoreAdapter)
        stbAdapterExprKnot =
            pureAppKnot $ MkAllFor $ \NilArguments -> invmap MkOpenEntity unOpenEntity plainStoreAdapter
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    in singleGroundType' (MkFamilialType openEntityFamilyWitness $ MkOpenEntityFamily oet) props $ exprShowPrec oet

getOpenEntityType :: Some QNonpolarType -> QInterpreter (Some OpenEntityType)
getOpenEntityType (MkSome (GroundedNonpolarType (MkNonpolarGroundedType gt NilCCRArguments)))
    | Just (MkOpenEntityFamily oet) <- getGroundFamily openEntityFamilyWitness gt = return $ MkSome oet
getOpenEntityType tm = throw $ InterpretTypeNotOpenEntityError $ exprShow tm
