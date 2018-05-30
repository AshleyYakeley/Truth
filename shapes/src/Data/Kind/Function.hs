{-# OPTIONS -fno-warn-orphans #-}

module Data.Kind.Function where

import Control.Category.Cartesian
import Control.Category.Combinator
import Data.Kind.Category
import Shapes.Import

data KindTerminal1 (p :: kp) =
    MkKindTerminal1

instance TerminalCategory (NestedMorphism :: (kp -> Type) -> (kp -> Type) -> Type) where
    type TerminalObject (NestedMorphism :: (kp -> Type) -> (kp -> Type) -> Type) = KindTerminal1
    terminal = MkNestedMorphism $ \_ -> MkKindTerminal1

data KindProduct1 (a :: kp -> Type) (b :: kp -> Type) (p :: kp) =
    MkKindProduct1 (a p)
                   (b p)

instance CategoryProduct (NestedMorphism :: (kp -> Type) -> (kp -> Type) -> Type) where
    type CatProduct (NestedMorphism :: (kp -> Type) -> (kp -> Type) -> Type) = KindProduct1
    fst' = MkNestedMorphism $ \(MkKindProduct1 ap _) -> ap
    snd' = MkNestedMorphism $ \(MkKindProduct1 _ bp) -> bp
    prod' (MkNestedMorphism ab) (MkNestedMorphism ac) = MkNestedMorphism $ \a -> MkKindProduct1 (ab a) (ac a)

instance CartesianClosedCategory (NestedMorphism :: (kp -> Type) -> (kp -> Type) -> Type) where
    type CatFunction (NestedMorphism :: (kp -> Type) -> (kp -> Type) -> Type) = KindFunction1
    catApplyPair = MkNestedMorphism $ \(MkKindProduct1 (MkKindFunction1 ab) a) -> ab a
    catCurry (MkNestedMorphism abc) = MkNestedMorphism $ \a -> MkKindFunction1 $ \b -> abc (MkKindProduct1 a b)
    catUncurry (MkNestedMorphism abc) = MkNestedMorphism $ \(MkKindProduct1 a b) -> unKindFunction1 (abc a) b

type KindFunction k (a :: k) (b :: k) = CatFunction (KindMorphism k) a b

type KindLimit k (a :: k) = CatObject (KindMorphism k) a

instance CategoryCombinator (NestedMorphism :: (kp -> Type) -> (kp -> Type) -> Type) where
    fromCombinator objf =
        MkNestedMorphism $
        case objf of
            MkNestedCatObject (Identity (MkKindFunction1 f)) -> f
    toCombinator mf =
        MkNestedCatObject $
        Identity $
        MkKindFunction1 $
        case mf of
            MkNestedMorphism f -> f

type FunctionKind k = CategoryCombinator (KindMorphism k)

newtype KindFunction1 (a :: kp -> Type) (b :: kp -> Type) (p :: kp) = MkKindFunction1
    { unKindFunction1 :: KindFunction Type (a p) (b p)
    }

{-
instance FunctionKind (kp -> Type) where
    type KindFunction (kp -> Type) = KindFunction1
    fromCombinator lf =
        MkNestedMorphism $
        case lf of
            MkNestedCatObject f -> unKindFunction1 $ runIdentity f
    toCombinator lf =
        MkNestedCatObject $
        case lf of
            MkNestedMorphism ff -> Identity $ MkKindFunction1 ff
    catConst = MkNestedMorphism $ \a -> MkKindFunction1 $ \_ -> a
    kfap =
        MkNestedMorphism $ \(MkKindFunction1 afbc) ->
            MkKindFunction1 $ \(MkKindFunction1 ab) -> MkKindFunction1 $ \a -> unKindFunction1 (afbc a) (ab a)
-}
newtype KindFunction2 (a :: kp -> kq -> Type) (b :: kp -> kq -> Type) (p :: kp) (q :: kq) = MkKindFunction2
    { unKindFunction2 :: KindFunction (kq -> Type) (a p) (b p) q
    }

{-
instance FunctionKind (kp -> kq -> Type) where
    type KindFunction (kp -> kq -> Type) = KindFunction2
    fromCombinator lf =
        MkNestedMorphism $
        MkNestedMorphism $
        case lf of
            MkNestedCatObject (MkNestedCatObject f) -> unKindFunction1 $ unKindFunction2 $ runIdentity f
    toCombinator lf =
        MkNestedCatObject $
        MkNestedCatObject $
        case lf of
            MkNestedMorphism (MkNestedMorphism f) -> Identity $ MkKindFunction2 $ MkKindFunction1 f
    catConst = MkNestedMorphism $ MkNestedMorphism $ \a -> MkKindFunction2 $ MkKindFunction1 $ \_ -> a
    kfap =
        MkNestedMorphism $
        MkNestedMorphism $ \(MkKindFunction2 (MkKindFunction1 afbc)) ->
            MkKindFunction2 $
            MkKindFunction1 $ \(MkKindFunction2 (MkKindFunction1 ab)) ->
                MkKindFunction2 $ MkKindFunction1 $ \a -> unKindFunction1 (unKindFunction2 (afbc a)) (ab a)
-}
class (CategoryCombinator (KindMorphism k), KindFunctor f) => KindApplicative (f :: k -> Type) where
    kpure :: KindLimit k a -> f a
    kap :: f (KindFunction k a b) -> f a -> f b
