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
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

unrollRecursiveMap ::
       forall (ground :: GroundTypeKind) polarity name p t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> (PolarMap (DolanPolyIsoShim ground Type) polarity (UVar Type name) (Recursive (UVar Type name) t) -> PolarMap (DolanPolyIsoShim ground Type) polarity t p)
    -> PolarMap (DolanPolyIsoShim ground Type) polarity (Recursive (UVar Type name) t) p
unrollRecursiveMap var convf = isoPolarPoly $ unrollRecursiveIsoShim var (polarPolyIso . convf . isoPolarPoly)

unrollRecursiveType ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity t
    -> DolanIsoShimWit ground polarity (Recursive (UVar Type name) t)
unrollRecursiveType var pt =
    case polarityType @polarity of
        PositiveType -> let
            t = singleDolanShimWit $ mkShimWit $ RecursiveDolanSingularType var pt
            dbisub =
                MkDeferredBisubstitution var (pure t) (pure $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType var)
            in case runIdentity $ deferBisubstituteType dbisub pt of
                   MkShimWit t' fconv ->
                       MkShimWit t' $ unrollRecursiveMap @ground var (\conv -> applyPolarPolyFuncShim fconv (conv, id))
        NegativeType -> let
            t = singleDolanShimWit $ mkShimWit $ RecursiveDolanSingularType var pt
            dbisub =
                MkDeferredBisubstitution var (pure $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType var) (pure t)
            in case runIdentity $ deferBisubstituteType dbisub pt of
                   MkShimWit t' fconv ->
                       MkShimWit t' $ unrollRecursiveMap @ground var (\conv -> applyPolarPolyFuncShim fconv (id, conv))

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
        :: forall (ground :: GroundTypeKind) polarity name t.
           SymbolType name
        -> DolanType ground polarity t
        -> RecursiveOrPlainType ground polarity (Recursive (UVar Type name) t)

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
unrollRecursiveOrPlainType (PlainType t) = mkShimWit t
unrollRecursiveOrPlainType (RecursiveType var t) = unrollRecursiveType var t

singularRecursiveOrPlainType ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) t.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> PShimWit shim (RecursiveOrPlainType ground) polarity t
singularRecursiveOrPlainType (RecursiveDolanSingularType var t) = mkShimWit $ RecursiveType var t
singularRecursiveOrPlainType st = chainShimWit (mkShimWit . PlainType) $ singleDolanShimWit $ mkShimWit st
