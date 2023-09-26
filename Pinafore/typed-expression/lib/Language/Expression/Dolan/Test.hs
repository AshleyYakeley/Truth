module Language.Expression.Dolan.Test
    ( recursiveDolanShimWit
    , Bisubstitution(..)
    , mkPolarBisubstitution
    , PShimWitMappable
    , bisubstitute
    , bisubstitutes
    , UnifierM
    , runUnifierM
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Solver.UnifierM
