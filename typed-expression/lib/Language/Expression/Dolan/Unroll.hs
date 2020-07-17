module Language.Expression.Dolan.Unroll
    ( unrollRecursiveType
    , unrollType
    , RecursiveOrPlainType(..)
    , unrollRecursiveOrPlainType
    , singularRecursiveOrPlainType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

unrollRecursiveType ::
       forall (ground :: GroundTypeKind) polarity name. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity (UVar Type name)
    -> DolanIsoShimWit ground polarity (UVar Type name)
unrollRecursiveType var t = runIdentity $ bisubstituteType (mkSingleBisubstitution var (pure $ mkShimWit t)) t

unrollSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanIsoShimWit ground polarity t
unrollSingularType (RecursiveDolanSingularType var t) = unrollRecursiveType var t
unrollSingularType t = singleDolanShimWit $ mkShimWit t

unrollType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanIsoShimWit ground polarity t
unrollType NilDolanType = mkShimWit NilDolanType
unrollType (ConsDolanType t1 tr) = joinMeetShimWit (unrollSingularType t1) (unrollType tr)

type RecursiveOrPlainType :: GroundTypeKind -> Polarity -> Type -> Type
data RecursiveOrPlainType ground polarity t where
    PlainType
        :: forall (ground :: GroundTypeKind) polarity t.
           DolanType ground polarity t
        -> RecursiveOrPlainType ground polarity t
    RecursiveType
        :: forall (ground :: GroundTypeKind) polarity name.
           SymbolType name
        -> DolanType ground polarity (UVar Type name)
        -> RecursiveOrPlainType ground polarity (UVar Type name)

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground) =>
             TestEquality (RecursiveOrPlainType ground polarity) where
    testEquality (PlainType ta) (PlainType tb) = testEquality ta tb
    testEquality (RecursiveType va ta) (RecursiveType vb tb) = do
        Refl <- testEquality va vb
        Refl <- testEquality ta tb
        return Refl
    testEquality _ _ = Nothing

unrollRecursiveOrPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => RecursiveOrPlainType ground polarity t
    -> DolanIsoShimWit ground polarity t
unrollRecursiveOrPlainType (PlainType pt) = mkShimWit pt
unrollRecursiveOrPlainType (RecursiveType var pt) = unrollRecursiveType var pt

singularRecursiveOrPlainType ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) t.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> PShimWit shim (RecursiveOrPlainType ground) polarity t
singularRecursiveOrPlainType (RecursiveDolanSingularType var t) = mkShimWit $ RecursiveType var t
singularRecursiveOrPlainType st = chainShimWit (mkShimWit . PlainType) $ singleDolanShimWit $ mkShimWit st
