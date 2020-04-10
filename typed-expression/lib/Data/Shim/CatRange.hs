module Data.Shim.CatRange where

import Data.Shim.JoinMeet
import Data.Shim.Polarity
import Data.Shim.Range
import Shapes

data CatRange (shim :: Type -> Type -> Type) (pq1 :: (Type, Type)) (pq2 :: (Type, Type)) where
    MkCatRange :: shim p2 p1 -> shim q1 q2 -> CatRange shim '( p1, q1) '( p2, q2)

catRangeContra :: CatRange shim pq1 pq2 -> shim (Contra pq2) (Contra pq1)
catRangeContra (MkCatRange s _) = s

catRangeCo :: CatRange shim pq1 pq2 -> shim (Co pq1) (Co pq2)
catRangeCo (MkCatRange _ s) = s

instance (forall a b. Show (shim a b)) => Show (CatRange shim a' b') where
    show (MkCatRange p q) = "(" <> show p <> "," <> show q <> ")"

coCatRange :: InCategory shim => shim q1 q2 -> CatRange shim '( p, q1) '( p, q2)
coCatRange qq = MkCatRange cid qq

contraCatRange :: InCategory shim => shim p2 p1 -> CatRange shim '( p1, q) '( p2, q)
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

liftCatRangeParts :: Functor f => CatRange (->) '( pa, qa) '( pb, qb) -> CatRange (->) '( f pa, f qa) '( f pb, f qb)
liftCatRangeParts (MkCatRange pp qq) = MkCatRange (cfmap pp) (cfmap qq)

data RangeType (tw :: Polarity -> Type -> Type) (polarity :: Polarity) (pq :: (Type, Type)) where
    MkRangeType :: tw (InvertPolarity polarity) p -> tw polarity q -> RangeType tw polarity '( p, q)

rangeTypeInKind :: forall tw polarity. Subrepresentative (RangeType tw polarity) (KindWitness (Type, Type))
rangeTypeInKind (MkRangeType _ _) = Dict

rangeToEnhanced :: EnhancedFunction shim => CatRange (->) a b -> CatRange shim a b
rangeToEnhanced (MkCatRange p q) = MkCatRange (toEnhanced "range-map" p) (toEnhanced "range-map" q)

instance EnhancedFunction shim => CatFunctor (CatRange (->)) (->) (Range shim a) where
    cfmap f = catRangeMap $ rangeToEnhanced f

coRangeLift ::
       forall f p q1 q2. CatFunctor (CatRange (->)) (->) f
    => (q1 -> q2)
    -> f '( p, q1)
    -> f '( p, q2)
coRangeLift f = cfmap (coCatRange f)

contraRangeLift ::
       forall f p1 p2 q. CatFunctor (CatRange (->)) (->) f
    => (p2 -> p1)
    -> f '( p1, q)
    -> f '( p2, q)
contraRangeLift f = cfmap (contraCatRange f)
