module Pinafore.Language.Type.Family where

import Shapes

type FamilyKind = forall (k :: Type). k -> Type

type SingletonFamily :: forall k. k -> FamilyKind
type SingletonFamily = HetEqual

type LiftedFamily :: forall ki. (ki -> Type) -> forall k. (ki -> k) -> FamilyKind
data LiftedFamily w f t where
    MkLiftedFamily :: forall ki (w :: ki -> Type) k (f :: ki -> k) (i :: ki). w i -> LiftedFamily w f (f i)

instance forall kw (w :: kw -> Type) (f :: kw -> Type). TestEquality w => TestHetEquality (LiftedFamily w f) where
    testHetEquality (MkLiftedFamily ia) (MkLiftedFamily ib) = do
        Refl <- testEquality ia ib
        return HRefl

newtype WitKind =
    MkWitKind FamilyKind

data FamilyType :: FamilyKind where
    MkFamilyType
        :: forall (fam :: FamilyKind) (k :: Type) (t :: k). TestHetEquality fam
        => IOWitness ('MkWitKind fam)
        -> fam t
        -> FamilyType t

matchFamilyType ::
       forall (fam :: FamilyKind) (k :: Type) (t :: k). IOWitness ('MkWitKind fam) -> FamilyType t -> Maybe (fam t)
matchFamilyType wit (MkFamilyType wit' t) = do
    Refl <- testEquality wit wit'
    return t

instance TestHetEquality FamilyType where
    testHetEquality (MkFamilyType wa ta) (MkFamilyType wb tb) = do
        Refl <- testEquality wa wb
        HRefl <- testHetEquality ta tb
        return HRefl
