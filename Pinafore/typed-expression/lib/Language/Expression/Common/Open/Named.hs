{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Open.Named where

import Shapes

import Language.Expression.Common.Open.NameWit

type UnitType :: Type -> Type -> Type
data UnitType a b where
    MkUnitType :: a -> UnitType a ()

instance Eq a => TestEquality (UnitType a) where
    testEquality (MkUnitType a1) (MkUnitType a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance Show a => Show (UnitType a b) where
    show (MkUnitType a) = show a

instance Show a => AllConstraint Show (UnitType a) where
    allConstraint = Dict

type UnitType' :: (Type -> Type) -> Type -> Type -> Type
data UnitType' a b c where
    MkUnitType' :: a c -> UnitType' a () c

instance Show (a c) => Show (UnitType' a b c) where
    show (MkUnitType' ac) = show ac

instance AllConstraint Show a => AllConstraint Show (UnitType' a b) where
    allConstraint :: forall (t :: Type). Dict (Show (UnitType' a b t))
    allConstraint =
        case allConstraint @_ @_ @Show @a @t of
            Dict -> Dict

instance AllConstraint Show a => AllConstraint (AllConstraint Show) (UnitType' a) where
    allConstraint = Dict

type NameWitness name w = NameTypeWitness (UnitType name) (UnitType' w)

pattern MkNameWitness :: name -> w t -> NameWitness name w t
pattern MkNameWitness name wit =
    MkNameTypeWitness (MkUnitType name) (MkUnitType' wit)

{-# COMPLETE MkNameWitness #-}

type NamedExpression :: Type -> (Type -> Type) -> Type -> Type
type NamedExpression name w = NameTypeExpression (UnitType name) (UnitType' w)

varNamedExpression :: name -> vw t -> NamedExpression name vw t
varNamedExpression n t = varNameTypeExpression (MkUnitType n) (MkUnitType' t)
