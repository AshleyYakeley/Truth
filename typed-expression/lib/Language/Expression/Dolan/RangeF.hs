module Language.Expression.Dolan.RangeF where

import Data.Shim.CatRange
import Data.Shim.Polarity
import Data.Shim.PolyMap
import Data.Shim.Range
import Data.Shim.ShimWit
import Language.Expression.Dolan.PShimWit
import Shapes

unToRangeShimWit ::
       forall (map :: MapKind Type) tw pa qa r. (FromShimWit map (tw 'Negative) pa, ToShimWit map (tw 'Positive) qa)
    => (forall pt qt. RangeType tw 'Positive '( pt, qt) -> CatRange map '( pa, qa) '( pt, qt) -> r)
    -> r
unToRangeShimWit cont =
    unNegShimWit fromShimWit $ \tp convp ->
        unPosShimWit toShimWit $ \tq convq -> cont (MkRangeType tp tq) (MkCatRange convp convq)

unFromRangeShimWit ::
       forall (map :: MapKind Type) tw pa qa r. (ToShimWit map (tw 'Positive) pa, FromShimWit map (tw 'Negative) qa)
    => (forall pt qt. RangeType tw 'Negative '( pt, qt) -> CatRange map '( pt, qt) '( pa, qa) -> r)
    -> r
unFromRangeShimWit cont =
    unPosShimWit toShimWit $ \tp convp ->
        unNegShimWit fromShimWit $ \tq convq -> cont (MkRangeType tp tq) (MkCatRange convp convq)

biRangeAnyF ::
       forall (map :: MapKind Type) tw t.
       (PShimWit map tw 'Negative t, PShimWit map tw 'Positive t)
    -> AnyF (RangeType tw 'Positive) (Range map t)
biRangeAnyF (sp, sq) =
    unNegShimWit sp $ \tp convp -> unPosShimWit sq $ \tq convq -> MkAnyF (MkRangeType tp tq) (MkRange convp convq)
