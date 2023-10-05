module Language.Expression.Dolan.Simplify.AutomateRecursion
    ( automateRecursion
    , automateRecursionInType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

automateRecursionInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanShimWit ground polarity t
automateRecursionInType t = mkShimWit t

automateRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => Endo a
automateRecursion = mapPShimWits @_ @(DolanType ground) automateRecursionInType automateRecursionInType
