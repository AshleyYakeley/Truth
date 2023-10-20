module Language.Expression.Dolan.Unroll
    ( unrollRecursiveType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

unrollRecursiveType ::
       forall (ground :: GroundTypeKind) polarity tv. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity tv
    -> DolanIsoShimWit ground polarity tv
unrollRecursiveType var pt =
    withInvertPolarity @polarity $
    singleBisubstitute var (shimWitToDolan $ mkPolarShimWit $ RecursiveDolanSingularType var pt) (mkPolarShimWit pt)
