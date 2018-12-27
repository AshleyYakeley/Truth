module Language.Expression.Dolan
    ( module I
    , module Language.Expression.Dolan
    ) where

import Language.Expression.Dolan.Arguments as I
import Language.Expression.Dolan.MPolarity as I
import Language.Expression.Dolan.Polarity as I
import Language.Expression.Dolan.Range as I
import Language.Expression.Dolan.RangeF as I
import Language.Expression.Dolan.TypeF as I
import Language.Expression.Dolan.Variance as I
import Language.Expression.UVar
import Shapes

data Bisubstitution (wit :: TypePolarity -> Type -> Type) =
    forall name. MkBisubstitution (SymbolWitness name)
                                  (TypeF wit 'PositivePolarity (UVar name))
                                  (TypeF wit 'NegativePolarity (UVar name))
