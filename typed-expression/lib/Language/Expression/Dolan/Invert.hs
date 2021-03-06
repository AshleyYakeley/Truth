module Language.Expression.Dolan.Invert
    ( invertTypeMaybe
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

minimalPositiveSupertypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
minimalPositiveSupertypeSingular (VarDolanSingularType v) = Just $ varDolanShimWit v
minimalPositiveSupertypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM invertTypeMaybe (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv
minimalPositiveSupertypeSingular (RecursiveDolanSingularType var t) = do
    t' <- minimalPositiveSupertype t
    return $ singleDolanShimWit $ recursiveDolanShimWit var t'

minimalPositiveSupertype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
minimalPositiveSupertype (ConsDolanType t NilDolanType) = do
    tf <- minimalPositiveSupertypeSingular t
    return $ ccontramap @_ @_ @(DolanShim ground) meet1 tf
minimalPositiveSupertype _ = Nothing

maximalNegativeSubtypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
maximalNegativeSubtypeSingular (VarDolanSingularType v) = Just $ varDolanShimWit v
maximalNegativeSubtypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM invertTypeMaybe (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv
maximalNegativeSubtypeSingular (RecursiveDolanSingularType var t) = do
    t' <- maximalNegativeSubtype t
    return $ singleDolanShimWit $ recursiveDolanShimWit var t'

maximalNegativeSubtype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
maximalNegativeSubtype (ConsDolanType t NilDolanType) = do
    tf <- maximalNegativeSubtypeSingular t
    return $ cfmap @_ @_ @(DolanShim ground) join1 tf
maximalNegativeSubtype _ = Nothing

invertTypeMaybe ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> Maybe (DolanShimWit ground (InvertPolarity polarity) a)
invertTypeMaybe =
    case polarityType @polarity of
        PositiveType -> maximalNegativeSubtype @ground
        NegativeType -> minimalPositiveSupertype @ground
