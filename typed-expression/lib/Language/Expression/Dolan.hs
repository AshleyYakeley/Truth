module Language.Expression.Dolan
    ( module I
    , module Language.Expression.Dolan
    ) where

import Data.Shim
import Language.Expression.Arguments as I
import Language.Expression.Dolan.Arguments as I
import Language.Expression.Dolan.Covariance as I
import Language.Expression.Dolan.MPolarity as I
import Language.Expression.Dolan.PShimWit as I
import Language.Expression.Dolan.RangeF as I
import Language.Expression.Dolan.Variance as I
import Language.Expression.UVar
import Shapes

data Bisubstitution (shim :: MapKind Type) m (wit :: Polarity -> Type -> Type) =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (PShimWit shim wit 'Positive (UVar name)))
                                  (m (PShimWit shim wit 'Negative (UVar name)))
