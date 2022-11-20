module Language.Expression.Dolan.FreeVars where

import Data.Shim
import Shapes

class FreeTypeVariables t where
    freeTypeVariables :: t -> FiniteSet (Some SymbolType)

instance FreeTypeVariables (Some SymbolType) where
    freeTypeVariables x = pure x

instance FreeTypeVariables (SymbolType n) where
    freeTypeVariables x = freeTypeVariables $ MkSome x

instance FreeTypeVariables t => FreeTypeVariables [t] where
    freeTypeVariables x = mconcat $ fmap freeTypeVariables x

instance FreeTypeVariables t => FreeTypeVariables (Maybe t) where
    freeTypeVariables (Just x) = freeTypeVariables x
    freeTypeVariables Nothing = mempty

instance (forall t'. FreeTypeVariables (wit t')) => FreeTypeVariables (ShimWit shim wit t) where
    freeTypeVariables (MkShimWit wt _) = freeTypeVariables wt
