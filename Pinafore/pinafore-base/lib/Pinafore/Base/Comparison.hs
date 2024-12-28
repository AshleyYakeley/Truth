module Pinafore.Base.Comparison where

import Shapes

newtype Equivalence a = MkEquivalence
    { equivalent :: a -> a -> Bool
    }

notEquivalent :: Equivalence a -> a -> a -> Bool
notEquivalent equiv p q = not $ equivalent equiv p q

eqEquivalence ::
       forall a. Eq a
    => Equivalence a
eqEquivalence = MkEquivalence (==)

instance RepresentationalRole Equivalence where
    representationalCoercion MkCoercion = MkCoercion

instance Contravariant Equivalence where
    contramap ba (MkEquivalence o) = MkEquivalence $ \p q -> o (ba p) (ba q)

newtype Preorder a = MkPreorder
    { comparePreorder :: a -> a -> Maybe Ordering
    }

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
       forall a. Ord a
    => Order a
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
        a :| (b:c) -> orderLeast order $ (orderLesser order a b) :| c

orderGreatest :: Order a -> NonEmpty a -> a
orderGreatest order =
    \case
        a :| [] -> a
        a :| (b:c) -> orderGreatest order $ (orderGreater order a b) :| c

orderSort :: Order a -> [a] -> [a]
orderSort (MkOrder o) = sortBy o
