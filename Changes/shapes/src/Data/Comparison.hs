{-# OPTIONS -fno-warn-orphans #-}

module Data.Comparison where

import Data.Reflection (Given (..), give)

import Data.Coerce.Coercion
import Data.Coerce.Role
import Data.Givable
import Data.Wrappable
import Shapes.Import

newtype Equivalence a = MkEquivalence
    { equivalent :: a -> a -> Bool
    }

notEquivalent :: Equivalence a -> a -> a -> Bool
notEquivalent equiv p q = not $ equivalent equiv p q

eqEquivalence ::
    forall a.
    Eq a =>
    Equivalence a
eqEquivalence = MkEquivalence (==)

nubByEquivalence :: forall a. Equivalence a -> [a] -> [a]
nubByEquivalence eqv = nubBy $ equivalent eqv

instance RepresentationalRole Equivalence where
    representationalCoercion MkCoercion = MkCoercion

instance Contravariant Equivalence where
    contramap ba (MkEquivalence o) = MkEquivalence $ \p q -> o (ba p) (ba q)

instance Invariant Equivalence where
    invmap _ qp = contramap qp

instance Summable Equivalence where
    rVoid = MkEquivalence $ \p -> never p
    MkEquivalence a <+++> MkEquivalence b = let
        ab (Left p) (Left q) = a p q
        ab (Right p) (Right q) = b p q
        ab _ _ = False
        in MkEquivalence ab

instance Productable Equivalence where
    rUnit = MkEquivalence $ \_ _ -> True
    MkEquivalence oa <***> MkEquivalence ob = MkEquivalence $ \(pa, pb) (qa, qb) -> oa pa qa && ob pb qb

instance Semigroup (Equivalence a) where
    oa <> ob = contramap (\a -> (a, a)) $ oa <***> ob

instance Monoid (Equivalence a) where
    mempty = contramap (\_ -> ()) rUnit

instance Given (Equivalence a) => Eq (Wrapper Type a) where
    (==) = coerce $ equivalent (given :: Equivalence a)

instance Givable Equivalence where
    type GivableConstraint Equivalence = Eq
    giveWrapperConstraint eqv call = give eqv call

newtype Preorder a = MkPreorder
    { comparePreorder :: a -> a -> Maybe Ordering
    }

instance Invariant Preorder where
    invmap _ qp = contramap qp

preorderEquivalence :: forall a. Preorder a -> Equivalence a
preorderEquivalence (MkPreorder o) = MkEquivalence $ \p q -> o p q == Just EQ

equivalencePreorder :: forall a. Equivalence a -> Preorder a
equivalencePreorder (MkEquivalence o) =
    MkPreorder $ \p q ->
        if o p q
            then Just EQ
            else Nothing

reversePreorder :: forall a. Preorder a -> Preorder a
reversePreorder (MkPreorder o) = MkPreorder $ \p q -> o q p

preorderEQ :: Preorder a -> a -> a -> Bool
preorderEQ o p q =
    case comparePreorder o p q of
        Just EQ -> True
        _ -> False

preorderLT :: Preorder a -> a -> a -> Bool
preorderLT o p q =
    case comparePreorder o p q of
        Just LT -> True
        _ -> False

preorderLE :: Preorder a -> a -> a -> Bool
preorderLE o p q =
    case comparePreorder o p q of
        Just LT -> True
        Just EQ -> True
        _ -> False

preorderGT :: Preorder a -> a -> a -> Bool
preorderGT o p q =
    case comparePreorder o p q of
        Just GT -> True
        _ -> False

preorderGE :: Preorder a -> a -> a -> Bool
preorderGE o p q =
    case comparePreorder o p q of
        Just GT -> True
        Just EQ -> True
        _ -> False

instance RepresentationalRole Preorder where
    representationalCoercion MkCoercion = MkCoercion

instance Contravariant Preorder where
    contramap ba (MkPreorder o) = MkPreorder $ \p q -> o (ba p) (ba q)

newtype Order a = MkOrder
    { compareOrder :: a -> a -> Ordering
    }

orderPreorder :: forall a. Order a -> Preorder a
orderPreorder (MkOrder o) = MkPreorder $ \p q -> Just $ o p q

orderEquivalence :: forall a. Order a -> Equivalence a
orderEquivalence (MkOrder o) = MkEquivalence $ \p q -> o p q == EQ

joinOrderings :: Ordering -> Ordering -> Ordering
joinOrderings EQ ob = ob
joinOrderings oa _ = oa

instance RepresentationalRole Order where
    representationalCoercion MkCoercion = MkCoercion

instance Contravariant Order where
    contramap ba (MkOrder o) = MkOrder $ \p q -> o (ba p) (ba q)

instance Invariant Order where
    invmap _ qp = contramap qp

instance Summable Order where
    rVoid = MkOrder $ \p -> never p
    MkOrder a <+++> MkOrder b = let
        ab (Left p) (Left q) = a p q
        ab (Right p) (Right q) = b p q
        ab (Left _) (Right _) = LT
        ab (Right _) (Left _) = GT
        in MkOrder ab

instance Productable Order where
    rUnit = MkOrder $ \_ _ -> EQ
    MkOrder oa <***> MkOrder ob = MkOrder $ \(pa, pb) (qa, qb) -> joinOrderings (oa pa qa) (ob pb qb)

instance Semigroup (Order a) where
    oa <> ob = contramap (\a -> (a, a)) $ oa <***> ob

instance Monoid (Order a) where
    mempty = contramap (\_ -> ()) rUnit

orderEQ :: Order a -> a -> a -> Bool
orderEQ o p q =
    case compareOrder o p q of
        EQ -> True
        _ -> False

orderLT :: Order a -> a -> a -> Bool
orderLT o p q =
    case compareOrder o p q of
        LT -> True
        _ -> False

orderLE :: Order a -> a -> a -> Bool
orderLE o p q =
    case compareOrder o p q of
        LT -> True
        EQ -> True
        _ -> False

orderGT :: Order a -> a -> a -> Bool
orderGT o p q =
    case compareOrder o p q of
        GT -> True
        _ -> False

orderGE :: Order a -> a -> a -> Bool
orderGE o p q =
    case compareOrder o p q of
        GT -> True
        EQ -> True
        _ -> False

ordOrder ::
    forall a.
    Ord a =>
    Order a
ordOrder = MkOrder compare

indiscreteOrder :: forall a. Order a
indiscreteOrder = MkOrder $ \_ _ -> EQ

reverseOrder :: forall a. Order a -> Order a
reverseOrder (MkOrder o) = MkOrder $ \p q -> o q p

orderLesser :: Order a -> a -> a -> a
orderLesser (MkOrder f) a b =
    case f a b of
        GT -> b
        _ -> a

orderGreater :: Order a -> a -> a -> a
orderGreater (MkOrder f) a b =
    case f a b of
        GT -> a
        _ -> b

orderLeast :: Order a -> NonEmpty a -> a
orderLeast order =
    \case
        a :| [] -> a
        a :| (b : c) -> orderLeast order $ (orderLesser order a b) :| c

orderGreatest :: Order a -> NonEmpty a -> a
orderGreatest order =
    \case
        a :| [] -> a
        a :| (b : c) -> orderGreatest order $ (orderGreater order a b) :| c

orderSort :: Order a -> [a] -> [a]
orderSort (MkOrder o) = sortBy o

instance (Given (Equivalence a), Given (Order a)) => Ord (Wrapper Type a) where
    compare = coerce $ compareOrder (given :: Order a)

instance Givable Order where
    type GivableConstraint Order = Ord
    giveWrapperConstraint order call = give (orderEquivalence order) $ give order call
