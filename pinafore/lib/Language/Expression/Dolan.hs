module Language.Expression.Dolan
    ( module I
    , typeFExpression
    ) where

import Language.Expression.Dolan.Polarity as I
import Language.Expression.Dolan.TypeRange as I
import Language.Expression.Dolan.Variance as I
import Language.Expression.Sealed
import Shapes

typeFExpression ::
       TypeF wit 'PositivePolarity t -> t -> SealedExpression name (wit 'NegativePolarity) (wit 'PositivePolarity)
typeFExpression (MkTypeF tt conv) t = constSealedExpression tt $ conv t
