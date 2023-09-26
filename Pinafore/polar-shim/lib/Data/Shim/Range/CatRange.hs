module Data.Shim.Range.CatRange where

import Data.Shim.Mono
import Data.Shim.Polar
import Data.Shim.Poly
import Data.Shim.Range.Range
import Data.Shim.Range.RangeType
import Shapes

data CatRange (shim :: ShimKind Type) (pq1 :: (Type, Type)) (pq2 :: (Type, Type)) where
    MkCatRange :: shim p2 p1 -> shim q1 q2 -> CatRange shim '( p1, q1) '( p2, q2)

catRangeContra :: CatRange shim pq1 pq2 -> shim (Contra pq2) (Contra pq1)
catRangeContra (MkCatRange s _) = s

catRangeCo :: CatRange shim pq1 pq2 -> shim (Co pq1) (Co pq2)
catRangeCo (MkCatRange _ s) = s

instance (forall a b. Show (shim a b)) => Show (CatRange shim a' b') where
    show (MkCatRange p q) = "(" <> show p <> "," <> show q <> ")"

coCatRange :: Category shim => shim q1 q2 -> CatRange shim '( p, q1) '( p, q2)
coCatRange qq = MkCatRange id qq

contraCatRange :: Category shim => shim p2 p1 -> CatRange shim '( p1, q) '( p2, q)
contraCatRange pp = MkCatRange pp id

instance Category shim => Category (CatRange shim) where
    id :: forall a. CatRange shim a a
    id =
        case unsafeTypeIsPair @_ @_ @a of
            Refl -> MkCatRange id id
    (.) :: forall a b c. CatRange shim b c -> CatRange shim a b -> CatRange shim a c
    MkCatRange pa qa . MkCatRange pb qb = MkCatRange (pb . pa) (qa . qb)

instance Groupoid cat => Groupoid (CatRange cat) where
    invert (MkCatRange p q) = MkCatRange (invert p) (invert q)

catRangeMap :: Category shim => CatRange shim a b -> Range shim t a -> Range shim t b
catRangeMap (MkCatRange pp qq) (MkRange pt tq) = MkRange (pt . pp) (qq . tq)

{-
liftCatRangeParts :: Functor f => CatRange (->) '( pa, qa) '( pb, qb) -> CatRange (->) '( f pa, f qa) '( f pb, f qb)
liftCatRangeParts (MkCatRange pp qq) = MkCatRange (cfmap pp) (cfmap qq)
-}
instance forall k (f :: (Type, Type) -> k). CatFunctor (CatRange (NullShim Type)) (NullShim k) f where
    cfmap :: forall a b. CatRange (NullShim Type) a b -> NullShim k (f a) (f b)
    cfmap (MkCatRange MkNullShim MkNullShim) = MkNullShim

rangeToEnhanced :: FunctionShim shim => CatRange (->) a b -> CatRange shim a b
rangeToEnhanced (MkCatRange p q) = MkCatRange (functionToShim "range-map" p) (functionToShim "range-map" q)

instance FunctionShim shim => CatFunctor (CatRange (->)) (->) (Range shim a) where
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

unToRangeShimWit ::
       forall (map :: ShimKind Type) tw pa qa r.
       (FromPolarShimWit map (tw 'Negative) pa, ToPolarShimWit map (tw 'Positive) qa)
    => (forall pt qt. RangeType tw 'Positive '( pt, qt) -> CatRange map '( pa, qa) '( pt, qt) -> r)
    -> r
unToRangeShimWit cont =
    unNegShimWit fromPolarShimWit $ \tp convp ->
        unPosShimWit toPolarShimWit $ \tq convq -> cont (MkRangeType tp tq) (MkCatRange convp convq)

unFromRangeShimWit ::
       forall (map :: ShimKind Type) tw pa qa r.
       (ToPolarShimWit map (tw 'Positive) pa, FromPolarShimWit map (tw 'Negative) qa)
    => (forall pt qt. RangeType tw 'Negative '( pt, qt) -> CatRange map '( pt, qt) '( pa, qa) -> r)
    -> r
unFromRangeShimWit cont =
    unPosShimWit toPolarShimWit $ \tp convp ->
        unNegShimWit fromPolarShimWit $ \tq convq -> cont (MkRangeType tp tq) (MkCatRange convp convq)

biRangeSomeFor ::
       forall (map :: ShimKind Type) tw t.
       (PShimWit map tw 'Negative t, PShimWit map tw 'Positive t)
    -> SomeFor (Range map t) (RangeType tw 'Positive)
biRangeSomeFor (sp, sq) =
    unNegShimWit sp $ \tp convp -> unPosShimWit sq $ \tq convq -> MkSomeFor (MkRangeType tp tq) (MkRange convp convq)
