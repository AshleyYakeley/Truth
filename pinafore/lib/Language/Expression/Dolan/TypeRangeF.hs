module Language.Expression.Dolan.TypeRangeF where

import Language.Expression.Dolan.Polarity
import Language.Expression.Dolan.TypeF
import Language.Expression.Dolan.TypeRange
import Shapes

unToWithTypeF ::
       (FromTypeF tw pa, ToTypeF tw qa)
    => (forall pt qt. TypeRangeWitness tw 'PositivePolarity '( pt, qt) -> WithRange (->) '( pa, qa) '( pt, qt) -> r)
    -> r
unToWithTypeF cont =
    unTypeF fromTypeF $ \tp convp ->
        unTypeF toTypeF $ \tq convq -> cont (MkTypeRangeWitness tp tq) (MkWithRange convp convq)

unFromWithTypeF ::
       (ToTypeF tw pa, FromTypeF tw qa)
    => (forall pt qt. TypeRangeWitness tw 'NegativePolarity '( pt, qt) -> WithRange (->) '( pt, qt) '( pa, qa) -> r)
    -> r
unFromWithTypeF cont =
    unTypeF toTypeF $ \tp convp ->
        unTypeF fromTypeF $ \tq convq -> cont (MkTypeRangeWitness tp tq) (MkWithRange convp convq)

biTypeF ::
       (TypeF tw 'NegativePolarity t, TypeF tw 'PositivePolarity t)
    -> AnyF (TypeRangeWitness tw 'PositivePolarity) (TypeRange t)
biTypeF (MkTypeF tp convp, MkTypeF tq convq) = MkAnyF (MkTypeRangeWitness tp tq) (MkTypeRange convp convq)
