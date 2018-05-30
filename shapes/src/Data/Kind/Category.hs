module Data.Kind.Category where

import Control.Category.Cartesian
import Control.Category.Combinator
import Shapes.Import

type family KindMorphism (k :: Type) :: k -> k -> Type

type instance KindMorphism Type = (->)

type instance KindMorphism Constraint = (:-)

newtype NestedMorphism (a :: kp -> kq) (b :: kp -> kq) =
    MkNestedMorphism (forall (p :: kp). KindMorphism kq (a p) (b p))

instance Category (KindMorphism kq) => Category (NestedMorphism :: (kp -> kq) -> (kp -> kq) -> Type) where
    id = MkNestedMorphism id
    (MkNestedMorphism bc) . (MkNestedMorphism ab) = MkNestedMorphism (bc . ab)

newtype NestedCatObject (a :: kp -> kq) =
    MkNestedCatObject (forall (p :: kp). KindObject kq (a p))

type instance KindMorphism (kp -> kq) =
     (NestedMorphism :: (kp -> kq) -> (kp -> kq) -> Type)

instance CategoryObject (KindMorphism kq) => CategoryObject (NestedMorphism :: (kp -> kq) -> (kp -> kq) -> Type) where
    type CatObject (NestedMorphism :: (kp -> kq) -> (kp -> kq) -> Type) = NestedCatObject
    catApply (MkNestedMorphism f) (MkNestedCatObject x) = MkNestedCatObject $ catApply f x
    catConst (MkNestedCatObject x) = MkNestedMorphism $ catConst x

type CategoryKind k = CategoryObject (KindMorphism k)

class CategoryKind k => KindFunctor (f :: k -> Type) where
    kfmap :: KindMorphism k a b -> f a -> f b

type KindObject (k :: Type) (a :: k) = CatObject (KindMorphism k) a

--type KindTerminal (k :: Type) = TerminalObject (KindMorphism k)
type KindProduct (k :: Type) (a :: k) (b :: k) = CatProduct (KindMorphism k) a b
