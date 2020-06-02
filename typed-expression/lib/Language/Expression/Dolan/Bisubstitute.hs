module Language.Expression.Dolan.Bisubstitute
    ( Bisubstitution(..)
    , DolanBisubstitution
    , bisubstitutePositiveSingularType
    , bisubstituteNegativeSingularType
    , bisubstitutesType
    , bisubstitutes
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.UVar
import Shapes

data Bisubstitution (shim :: MapKind Type) m (wit :: Polarity -> Type -> Type) =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (PShimWit shim wit 'Positive (UVar name)))
                                  (m (PShimWit shim wit 'Negative (UVar name)))

type DolanBisubstitution :: GroundTypeKind -> (Type -> Type) -> Type
type DolanBisubstitution ground m = Bisubstitution (DolanPolyShim ground Type) m (DolanType ground)

bisubstitutePositiveSingularType ::
       forall (ground :: GroundTypeKind) m t. (IsDolanGroundType ground, Monad m)
    => DolanBisubstitution ground m
    -> DolanSingularType ground 'Positive t
    -> m (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive t)
bisubstitutePositiveSingularType (MkBisubstitution n tp _) (VarDolanSingularType n')
    | Just Refl <- testEquality n n' = tp
bisubstitutePositiveSingularType _ t@(VarDolanSingularType _) = return $ singleDolanShimWit $ mkShimWit t
bisubstitutePositiveSingularType bisub (GroundDolanSingularType gt args) = let
    dvt = groundTypeVarianceType gt
    in do
           MkShimWit args' conv <- mapDolanArgumentsM (bisubstituteType bisub) dvt (groundTypeVarianceMap gt) args
           return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv

bisubstituteNegativeSingularType ::
       forall (ground :: GroundTypeKind) m t. (IsDolanGroundType ground, Monad m)
    => DolanBisubstitution ground m
    -> DolanSingularType ground 'Negative t
    -> m (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative t)
bisubstituteNegativeSingularType (MkBisubstitution n _ tq) (VarDolanSingularType n')
    | Just Refl <- testEquality n n' = tq
bisubstituteNegativeSingularType _ t@(VarDolanSingularType _) = return $ singleDolanShimWit $ mkShimWit t
bisubstituteNegativeSingularType bisub (GroundDolanSingularType gt args) = let
    dvt = groundTypeVarianceType gt
    in do
           MkShimWit args' conv <- mapDolanArgumentsM (bisubstituteType bisub) dvt (groundTypeVarianceMap gt) args
           return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv

bisubstitutePositiveType ::
       forall (ground :: GroundTypeKind) m t. (IsDolanGroundType ground, Monad m)
    => DolanBisubstitution ground m
    -> DolanType ground 'Positive t
    -> m (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive t)
bisubstitutePositiveType _ NilDolanType = return $ mkShimWit NilDolanType
bisubstitutePositiveType bisub (ConsDolanType ta tb) = do
    tfa <- bisubstitutePositiveSingularType bisub ta
    tfb <- bisubstitutePositiveType bisub tb
    return $ joinMeetDolanShimWit tfa tfb

bisubstituteNegativeType ::
       forall (ground :: GroundTypeKind) m t. (IsDolanGroundType ground, Monad m)
    => DolanBisubstitution ground m
    -> DolanType ground 'Negative t
    -> m (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative t)
bisubstituteNegativeType _ NilDolanType = return $ mkShimWit NilDolanType
bisubstituteNegativeType bisub (ConsDolanType ta tb) = do
    tfa <- bisubstituteNegativeSingularType bisub ta
    tfb <- bisubstituteNegativeType bisub tb
    return $ joinMeetDolanShimWit tfa tfb

bisubstituteType ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, Monad m, Is PolarityType polarity)
    => DolanBisubstitution ground m
    -> DolanType ground polarity t
    -> m (PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity t)
bisubstituteType =
    case polarityType @polarity of
        PositiveType -> bisubstitutePositiveType
        NegativeType -> bisubstituteNegativeType

bisubstitutesType ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, Monad m, Is PolarityType polarity)
    => [DolanBisubstitution ground m]
    -> DolanType ground polarity t
    -> m (PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity t)
bisubstitutesType [] t = return $ mkShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainShimWitM (bisubstitutesType subs) tf

bisubstitutes ::
       forall (ground :: GroundTypeKind) m a.
       (IsDolanGroundType ground, Monad m, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => [DolanBisubstitution ground m]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- mapPShimWitsM (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr
    bisubstitutes subs expr'
