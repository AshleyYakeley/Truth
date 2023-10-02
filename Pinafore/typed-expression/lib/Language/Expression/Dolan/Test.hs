module Language.Expression.Dolan.Test
    ( recursiveDolanShimWit
    , Bisubstitution(..)
    , mkPolarBisubstitution
    , PShimWitMappable
    , bisubstitute
    , bisubstitutes
    , TypeResult
    , runTypeResult
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.TypeResult
