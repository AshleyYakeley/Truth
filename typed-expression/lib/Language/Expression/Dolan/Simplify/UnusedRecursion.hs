module Language.Expression.Dolan.Simplify.UnusedRecursion
    ( eliminateUnusedRecursion
    ) where

import Data.Shim
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.VarSubstitute
import Shapes

elimInType ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Maybe (SymbolType name)
    -> DolanType ground polarity t
    -> DolanShimWit ground polarity t
elimInType _ NilDolanType = nilDolanShimWit
elimInType mn@(Just rn) (ConsDolanType (VarDolanSingularType n) tr)
    | Just Refl <- testEquality rn n = joinMeetShimWit (unsafeDeleteVarShimWit n) (elimInType mn tr)
elimInType mn (ConsDolanType t1 tr) = consDolanShimWit (mapDolanSingularType (elimInType Nothing) t1) (elimInType mn tr)

eliminateUnusedRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => a
    -> a
eliminateUnusedRecursion = mapPShimWits @_ @(DolanType ground) (elimInType Nothing) (elimInType Nothing)
