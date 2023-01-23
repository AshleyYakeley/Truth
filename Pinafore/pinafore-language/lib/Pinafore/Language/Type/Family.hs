module Pinafore.Language.Type.Family where

import Shapes

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
