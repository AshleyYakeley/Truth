module Language.Expression.Dolan.Simplify.Safety where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Solver.Safety
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

checkSafetyInType ::
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    Text ->
    DolanType ground polarity t ->
    DolanRenameTypeM ground (DolanShimWit ground polarity t)
checkSafetyInType msg t = do
    case checkSafety t of
        SuccessResult () -> return $ mkShimWit t
        FailureResult err -> throwExc $ InternalSafetyTypeError msg err t

checkSafetyMappable ::
    forall (ground :: GroundTypeKind) a.
    (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a) =>
    Text ->
    EndoM (DolanRenameTypeM ground) a
checkSafetyMappable msg = mapPShimWitsM (checkSafetyInType msg) (checkSafetyInType msg)
