module Language.Expression.Common.Pattern
    ( module I
    , module Language.Expression.Common.Pattern
    ) where

import Language.Expression.Common.Expression
import Language.Expression.Common.Pattern.Constructor as I
import Language.Expression.Common.Pattern.Expression as I
import Language.Expression.Common.Pattern.Named as I
import Language.Expression.Common.Pattern.Pattern as I
import Language.Expression.Common.Pattern.Sealed as I
import Shapes

matchPattern ::
       Pattern wp a b
    -> Expression we a
    -> (forall f t. PurityType Maybe f -> Expression we (f (t, b)) -> [SomeFor ((->) t) wp] -> r)
    -> r
matchPattern (MkPattern ww (MkPurityFunction purity (Kleisli qmt))) expr call = call purity (fmap qmt expr) ww
