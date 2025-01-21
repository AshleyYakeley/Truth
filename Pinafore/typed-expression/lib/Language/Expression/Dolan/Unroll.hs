module Language.Expression.Dolan.Unroll
    ( unrollRecursiveType
    , unrollTopType
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

unrollRecursiveType ::
    forall (ground :: GroundTypeKind) polarity tv.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TypeVarT tv ->
    DolanType ground polarity tv ->
    DolanIsoShimWit ground polarity tv
unrollRecursiveType var pt =
    withInvertPolarity @polarity
        $ singleBisubstitute var (shimWitToDolan $ mkPolarShimWit $ RecursiveDolanSingularType var pt) (mkPolarShimWit pt)

unrollTopSinglularType ::
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    DolanSingularType ground polarity t ->
    DolanIsoShimWit ground polarity t
unrollTopSinglularType (RecursiveDolanSingularType var pt) = chainShimWit unrollTopType $ unrollRecursiveType var pt
unrollTopSinglularType t = typeToDolan t

unrollTopType ::
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    DolanType ground polarity t ->
    DolanIsoShimWit ground polarity t
unrollTopType NilDolanType = mkShimWit NilDolanType
unrollTopType (ConsDolanType t1 tr) = joinMeetShimWit (unrollTopSinglularType t1) (unrollTopType tr)
