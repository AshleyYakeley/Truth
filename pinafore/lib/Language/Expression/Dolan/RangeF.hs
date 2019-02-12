module Language.Expression.Dolan.RangeF where

import Language.Expression.Dolan.PType
import Language.Expression.Dolan.Range
import Language.Expression.Polarity
import Language.Expression.TypeF
import Shapes

unToWithTypeF ::
       (FromTypeF (tw 'Negative) pa, ToTypeF (tw 'Positive) qa)
    => (forall pt qt. RangeType tw 'Positive '( pt, qt) -> WithRange (->) '( pa, qa) '( pt, qt) -> r)
    -> r
unToWithTypeF cont =
    unTypeF fromTypeF $ \tp convp -> unTypeF toTypeF $ \tq convq -> cont (MkRangeType tp tq) (MkWithRange convp convq)

unFromWithTypeF ::
       (ToTypeF (tw 'Positive) pa, FromTypeF (tw 'Negative) qa)
    => (forall pt qt. RangeType tw 'Negative '( pt, qt) -> WithRange (->) '( pt, qt) '( pa, qa) -> r)
    -> r
unFromWithTypeF cont =
    unTypeF toTypeF $ \tp convp -> unTypeF fromTypeF $ \tq convq -> cont (MkRangeType tp tq) (MkWithRange convp convq)

biTypeF :: (PTypeF tw 'Negative t, PTypeF tw 'Positive t) -> AnyF (RangeType tw 'Positive) (Range t)
biTypeF (MkTypeF tp convp, MkTypeF tq convq) = MkAnyF (MkRangeType tp tq) (MkRange convp convq)
