module Pinafore.Language.Type.Family where

import Import
import Pinafore.Language.Type.Identified

type FamilyKind = forall (k :: Type). k -> Type

type SingletonFamily :: forall k. k -> FamilyKind
type SingletonFamily = HetEqual

newtype WitKind =
    MkWitKind FamilyKind

data FamilialType :: FamilyKind where
    MkFamilialType
        :: forall (fam :: FamilyKind) (k :: Type) (t :: k). TestHetEquality fam
        => IOWitness ('MkWitKind fam)
        -> fam t
        -> FamilialType t

matchFamilyType ::
       forall (fam :: FamilyKind) (k :: Type) (t :: k). IOWitness ('MkWitKind fam) -> FamilialType t -> Maybe (fam t)
matchFamilyType wit (MkFamilialType wit' t) = do
    Refl <- testEquality wit wit'
    return t

instance TestHetEquality FamilialType where
    testHetEquality (MkFamilialType wa ta) (MkFamilialType wb tb) = do
        Refl <- testEquality wa wb
        HRefl <- testHetEquality ta tb
        return HRefl

instance forall (k :: Type) (t :: k). Eq (FamilialType t) where
    a == b = isJust $ testHetEquality a b

data SomeFamilialType =
    forall (k :: Type) (t :: k). MkSomeFamilialType (FamilialType t)

instance Eq SomeFamilialType where
    MkSomeFamilialType a == MkSomeFamilialType b = isJust $ testHetEquality a b

data IdentifiedTypeFamily :: FamilyKind where
    MkIdentifiedTypeFamily :: forall (tid :: Nat). TypeIDType tid -> IdentifiedTypeFamily (Identified tid)

instance TestHetEquality IdentifiedTypeFamily where
    testHetEquality (MkIdentifiedTypeFamily ia) (MkIdentifiedTypeFamily ib) = do
        Refl <- testEquality ia ib
        return HRefl

identifiedFamilyWitness :: IOWitness ('MkWitKind IdentifiedTypeFamily)
identifiedFamilyWitness = $(iowitness [t|'MkWitKind IdentifiedTypeFamily|])

identifiedFamilialType :: forall (tid :: Nat). TypeIDType tid -> FamilialType (Identified tid)
identifiedFamilialType typeID = MkFamilialType identifiedFamilyWitness $ MkIdentifiedTypeFamily typeID
