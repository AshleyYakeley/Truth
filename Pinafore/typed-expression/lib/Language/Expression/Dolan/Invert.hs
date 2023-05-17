module Language.Expression.Dolan.Invert
    ( invertTypeMaybe
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

invertPositiveGrounded ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanGroundedType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
invertPositiveGrounded (MkDolanGroundedType gt args) = do
    MkShimWit args' conv <- mapInvertDolanArgumentsM invertPolarType (groundTypeVarianceMap gt) args
    return $ shimWitToDolan $ MkShimWit (MkDolanGroundedType gt args') conv

invertPositiveSingular ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
invertPositiveSingular (VarDolanSingularType v) =
    case ?rigidity $ typeVarName v of
        FreeName -> Just $ varDolanShimWit v
        RigidName -> Nothing
invertPositiveSingular (GroundedDolanSingularType t) = invertPositiveGrounded t
invertPositiveSingular (RecursiveDolanSingularType var t) = do
    t' <- invertPositiveType t
    return $ shimWitToDolan $ recursiveDolanShimWit var t'

invertPositiveType ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
invertPositiveType (ConsDolanType t NilDolanType) = do
    tf <- invertPositiveSingular t
    return $ mapPosShimWit meet1 tf
invertPositiveType _ = Nothing

invertNegativeGrounded ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanGroundedType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
invertNegativeGrounded (MkDolanGroundedType gt args) = do
    MkShimWit args' conv <- mapInvertDolanArgumentsM invertPolarType (groundTypeVarianceMap gt) args
    return $ shimWitToDolan $ MkShimWit (MkDolanGroundedType gt args') conv

invertNegativeSingular ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
invertNegativeSingular (VarDolanSingularType v) =
    case ?rigidity $ typeVarName v of
        FreeName -> Just $ varDolanShimWit v
        RigidName -> Nothing
invertNegativeSingular (GroundedDolanSingularType t) = invertNegativeGrounded t
invertNegativeSingular (RecursiveDolanSingularType var t) = do
    t' <- invertNegativeType t
    return $ shimWitToDolan $ recursiveDolanShimWit var t'

invertNegativeType ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
invertNegativeType (ConsDolanType t NilDolanType) = do
    tf <- invertNegativeSingular t
    return $ mapNegShimWit join1 tf
invertNegativeType _ = Nothing

invertPolarType ::
       forall (ground :: GroundTypeKind) polarity a.
       (IsDolanSubtypeGroundType ground, Is PolarityType polarity, ?rigidity :: String -> NameRigidity)
    => DolanType ground polarity a
    -> Maybe (DolanShimWit ground (InvertPolarity polarity) a)
invertPolarType =
    case polarityType @polarity of
        PositiveType -> invertNegativeType @ground
        NegativeType -> invertPositiveType @ground

invertTypeMaybe ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => (String -> NameRigidity)
    -> DolanType ground polarity a
    -> Maybe (DolanShimWit ground (InvertPolarity polarity) a)
invertTypeMaybe rigidity = let
    ?rigidity = rigidity
    in invertPolarType @ground
