module Language.Expression.Sealed where

import Language.Expression.Expression
import Shapes

data SealedExpression vw tw =
    forall t. MkSealedExpression (tw t)
                                 (Expression vw t)

letSealedExpression ::
       (forall t t'. tw t -> vw t' -> Maybe (t -> t'))
    -> SealedExpression vw tw
    -> SealedExpression vw tw
    -> SealedExpression vw tw
letSealedExpression match (MkSealedExpression twv val) (MkSealedExpression twt body) =
    MkSealedExpression twt $ letExpression (match twv) val body
