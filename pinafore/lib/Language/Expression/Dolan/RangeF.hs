module Language.Expression.Dolan.RangeF where

import Language.Expression.Dolan.Polarity
import Language.Expression.Dolan.Range
import Language.Expression.Dolan.TypeF
import Shapes

unToWithTypeF ::
       (FromTypeF tw pa, ToTypeF tw qa)
    => (forall pt qt. RangeType tw 'PositivePolarity '( pt, qt) -> WithRange (->) '( pa, qa) '( pt, qt) -> r)
    -> r
unToWithTypeF cont =
    unTypeF fromTypeF $ \tp convp -> unTypeF toTypeF $ \tq convq -> cont (MkRangeType tp tq) (MkWithRange convp convq)

unFromWithTypeF ::
       (ToTypeF tw pa, FromTypeF tw qa)
    => (forall pt qt. RangeType tw 'NegativePolarity '( pt, qt) -> WithRange (->) '( pt, qt) '( pa, qa) -> r)
    -> r
unFromWithTypeF cont =
    unTypeF toTypeF $ \tp convp -> unTypeF fromTypeF $ \tq convq -> cont (MkRangeType tp tq) (MkWithRange convp convq)

biTypeF ::
       (TypeF tw 'NegativePolarity t, TypeF tw 'PositivePolarity t) -> AnyF (RangeType tw 'PositivePolarity) (Range t)
biTypeF (MkTypeF tp convp, MkTypeF tq convq) = MkAnyF (MkRangeType tp tq) (MkRange convp convq)
