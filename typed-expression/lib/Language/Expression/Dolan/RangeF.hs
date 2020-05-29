module Language.Expression.Dolan.RangeF where

import Data.Shim.CatRange
import Data.Shim.JMShim
import Data.Shim.JMShimWit
import Data.Shim.Polarity
import Data.Shim.Range
import Data.Shim.ShimWit
import Language.Expression.Dolan.PShimWit
import Shapes

unToRangeShimWit ::
       forall tw pa qa r. (FromShimWit JMShim (tw 'Negative) pa, ToShimWit JMShim (tw 'Positive) qa)
    => (forall pt qt. RangeType tw 'Positive '( pt, qt) -> CatRange JMShim '( pa, qa) '( pt, qt) -> r)
    -> r
unToRangeShimWit cont =
    unNegShimWit fromJMShimWit $ \tp convp ->
        unPosShimWit toJMShimWit $ \tq convq -> cont (MkRangeType tp tq) (MkCatRange convp convq)

unFromRangeShimWit ::
       forall tw pa qa r. (ToShimWit JMShim (tw 'Positive) pa, FromShimWit JMShim (tw 'Negative) qa)
    => (forall pt qt. RangeType tw 'Negative '( pt, qt) -> CatRange JMShim '( pt, qt) '( pa, qa) -> r)
    -> r
unFromRangeShimWit cont =
    unPosShimWit toJMShimWit $ \tp convp ->
        unNegShimWit fromJMShimWit $ \tq convq -> cont (MkRangeType tp tq) (MkCatRange convp convq)

biRangeAnyF :: (PJMShimWit tw 'Negative t, PJMShimWit tw 'Positive t) -> AnyF (RangeType tw 'Positive) (Range JMShim t)
biRangeAnyF (sp, sq) =
    unNegShimWit sp $ \tp convp -> unPosShimWit sq $ \tq convq -> MkAnyF (MkRangeType tp tq) (MkRange convp convq)
