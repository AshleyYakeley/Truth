module Data.Shim.CatRange where

import Data.Shim.Polarity
import Data.Shim.Range
import Shapes

data CatRange (shim :: k -> k -> Type) (pq1 :: (k, k)) (pq2 :: (k, k)) where
    MkCatRange
        :: (InKind p1, InKind p2, InKind q1, InKind q2)
        => shim p2 p1
        -> shim q1 q2
        -> CatRange shim '( p1, q1) '( p2, q2)

instance (forall a b. Show (shim a b)) => Show (CatRange shim a' b') where
    show (MkCatRange p q) = "(" <> show p <> "," <> show q <> ")"

coCatRange :: (InCategory shim, InKind p, InKind q1, InKind q2) => shim q1 q2 -> CatRange shim '( p, q1) '( p, q2)
coCatRange qq = MkCatRange cid qq

contraCatRange :: (InCategory shim, InKind p1, InKind p2, InKind q) => shim p2 p1 -> CatRange shim '( p1, q) '( p2, q)
contraCatRange pp = MkCatRange pp cid

instance InCategory shim => InCategory (CatRange shim) where
    cid :: forall a. InKind a
        => CatRange shim a a
    cid =
        case inKind @_ @a of
            MkPairWitness -> MkCatRange cid cid
    (<.>) ::
           forall a b c. (InKind a, InKind b, InKind c)
        => CatRange shim b c
        -> CatRange shim a b
        -> CatRange shim a c
    MkCatRange pa qa <.> MkCatRange pb qb =
        case (inKind @_ @a, inKind @_ @b, inKind @_ @c) of
            (MkPairWitness, MkPairWitness, MkPairWitness) -> MkCatRange (pb <.> pa) (qa <.> qb)

instance InGroupoid cat => InGroupoid (CatRange cat) where
    cinvert (MkCatRange p q) = MkCatRange (cinvert p) (cinvert q)

catRangeMap :: InCategory shim => CatRange shim a b -> Range shim t a -> Range shim t b
catRangeMap (MkCatRange pp qq) (MkRange pt tq) = MkRange (pt <.> pp) (qq <.> tq)

data RangeType (tw :: Polarity -> Type -> Type) (polarity :: Polarity) (pq :: (Type, Type)) where
    MkRangeType :: tw (InvertPolarity polarity) p -> tw polarity q -> RangeType tw polarity '( p, q)

rangeTypeInKind :: forall tw polarity. Subrepresentative (RangeType tw polarity) (KindWitness (Type, Type))
rangeTypeInKind (MkRangeType _ _) = Dict
