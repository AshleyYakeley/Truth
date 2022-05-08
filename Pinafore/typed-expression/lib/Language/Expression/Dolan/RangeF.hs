module Language.Expression.Dolan.RangeF where

import Data.Shim
import Language.Expression.Dolan.PShimWit
import Shapes

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
