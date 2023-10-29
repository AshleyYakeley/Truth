module Language.Expression.Dolan.Simplify.Safety where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.Safety
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

checkSafetyInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => Text
    -> DolanType ground polarity t
    -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
checkSafetyInType msg t = do
    case checkSafety t of
        SuccessResult () -> return $ mkShimWit t
        FailureResult err -> lift $ throwTypeError @ground $ InternalSafetyError msg err t

checkSafetyMappable ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => Text
    -> EndoM (DolanTypeCheckM ground) a
checkSafetyMappable msg = mapPShimWitsM (checkSafetyInType msg) (checkSafetyInType msg)
