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
import Language.Expression.Dolan.TypeMappable as I
import Language.Expression.Dolan.Variance as I
import Language.Expression.UVar
import Shapes

data Bisubstitution m (wit :: TypePolarity -> Type -> Type) =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (TypeF wit 'PositivePolarity (UVar name)))
                                  (m (TypeF wit 'NegativePolarity (UVar name)))
