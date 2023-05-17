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
       forall (ground :: GroundTypeKind) polarity tv. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity tv
    -> DolanIsoShimWit ground polarity tv
unrollRecursiveType var pt =
    invertPolarity @polarity $ let
        rt = RecursiveDolanSingularType var pt
        bisub :: Bisubstitution ground (DolanPolyIsoShim ground Type) Identity
        bisub = mkSingleBisubstitution True var $ return $ shimWitToDolan $ mkPolarShimWit rt
        in runIdentity $ bisubstituteType bisub pt

unrollSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> DolanIsoShimWit ground polarity t
unrollSingularType (RecursiveDolanSingularType var t) = unrollRecursiveType var t
unrollSingularType t = shimWitToDolan $ mkPolarShimWit t

unrollType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanIsoShimWit ground polarity t
unrollType NilDolanType = nilDolanShimWit
unrollType (ConsDolanType t1 tr) = joinMeetShimWit (unrollSingularType t1) (unrollType tr)

type RecursiveOrPlainType :: GroundTypeKind -> Polarity -> Type -> Type
data RecursiveOrPlainType ground polarity t where
    PlainType
        :: forall (ground :: GroundTypeKind) polarity t.
           DolanType ground polarity t
        -> RecursiveOrPlainType ground polarity t
    RecursiveType
        :: forall (ground :: GroundTypeKind) polarity tv.
           TypeVarT tv
        -> DolanType ground polarity tv
        -> RecursiveOrPlainType ground polarity tv

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
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
unrollRecursiveOrPlainType (PlainType t) = mkPolarShimWit t
unrollRecursiveOrPlainType (RecursiveType var t) = unrollRecursiveType var t

singularRecursiveOrPlainType ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) t.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> PShimWit shim (RecursiveOrPlainType ground) polarity t
singularRecursiveOrPlainType (RecursiveDolanSingularType var t) = mkPolarShimWit $ RecursiveType var t
singularRecursiveOrPlainType st = chainPolarShimWit (mkPolarShimWit . PlainType) $ shimWitToDolan $ mkPolarShimWit st
