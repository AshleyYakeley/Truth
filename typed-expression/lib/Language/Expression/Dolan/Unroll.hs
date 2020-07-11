module Language.Expression.Dolan.Unroll
    ( dolanTypeToPlainUnroll
    ) where

import Data.Shim
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

unrollSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Bisubstitution ground Identity
    -> DolanSingularType ground polarity t
    -> DolanSemiIsoPlainShimWit ground polarity t
unrollSingularType (MkBisubstitution n _ _) (VarDolanSingularType n')
    | Just Refl <- testEquality n n' = unsafeDeleteVarPlainShimWit n
unrollSingularType sub t =
    singleDolanPlainShimWit $ mapDolanSingularType (\wt -> runIdentity $ bisubstituteType sub wt) t

unrollPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Bisubstitution ground Identity
    -> DolanPlainType ground polarity t
    -> DolanSemiIsoPlainShimWit ground polarity t
unrollPlainType _ NilDolanPlainType = mkShimWit NilDolanPlainType
unrollPlainType sub (ConsDolanPlainType t1 tr) =
    joinMeetSemiIsoShimWit (unrollSingularType sub t1) (unrollPlainType sub tr)

dolanTypeToPlainUnroll ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanSemiIsoPlainShimWit ground polarity t
dolanTypeToPlainUnroll (PlainDolanType pt) = mkShimWit pt
dolanTypeToPlainUnroll t@(RecursiveDolanType var pt) =
    unrollPlainType (mkSingleBisubstitution var (pure $ mkShimWit t)) pt
