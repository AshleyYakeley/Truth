module Language.Expression.Dolan
    ( module I
    , module Language.Expression.Dolan
    ) where

import Language.Expression.Dolan.Arguments as I
import Language.Expression.Dolan.JoinMeet as I
import Language.Expression.Dolan.MPolarity as I
import Language.Expression.Dolan.Range as I
import Language.Expression.Dolan.RangeF as I
import Language.Expression.Dolan.TypeMappable as I
import Language.Expression.Dolan.Variance as I
import Language.Expression.Polarity
import Language.Expression.TypeF
import Language.Expression.UVar
import Shapes

data Bisubstitution m (wit :: Polarity -> Type -> Type) =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (TypeF wit 'Positive (UVar name)))
                                  (m (TypeF wit 'Negative (UVar name)))
