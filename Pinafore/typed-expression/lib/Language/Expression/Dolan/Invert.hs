module Language.Expression.Dolan.Invert
    ( invertTypeM
    , invertType
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Shim
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

invertPositiveGrounded ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, ?rigidity :: String -> NameRigidity) =>
    DolanGroundedType ground 'Negative a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) 'Positive a)
invertPositiveGrounded (MkDolanGroundedType gt args) = do
    args' <- mapInvertDolanArgumentsM invertPolarType (groundTypeVarianceMap gt) args
    return $ shimWitToDolan $ mkDolanGroundedShimWit gt args'

invertPositiveSingular ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, ?rigidity :: String -> NameRigidity) =>
    DolanSingularType ground 'Negative a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) 'Positive a)
invertPositiveSingular (VarDolanSingularType v) =
    case ?rigidity $ typeVarName v of
        FreeName -> Just $ varDolanShimWit v
        RigidName -> Nothing
invertPositiveSingular (GroundedDolanSingularType t) = invertPositiveGrounded t
invertPositiveSingular (RecursiveDolanSingularType var t) = do
    t' <- invertPositiveType t
    return $ shimWitToDolan $ recursiveDolanShimWit var t'

invertPositiveType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, ?rigidity :: String -> NameRigidity) =>
    DolanType ground 'Negative a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) 'Positive a)
invertPositiveType (ConsDolanType t NilDolanType) = do
    tf <- invertPositiveSingular t
    return $ mapPosShimWit iMeetL1 tf
invertPositiveType _ = Nothing

invertNegativeGrounded ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, ?rigidity :: String -> NameRigidity) =>
    DolanGroundedType ground 'Positive a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) 'Negative a)
invertNegativeGrounded (MkDolanGroundedType gt args) = do
    args' <- mapInvertDolanArgumentsM invertPolarType (groundTypeVarianceMap gt) args
    return $ shimWitToDolan $ mkDolanGroundedShimWit gt args'

invertNegativeSingular ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, ?rigidity :: String -> NameRigidity) =>
    DolanSingularType ground 'Positive a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) 'Negative a)
invertNegativeSingular (VarDolanSingularType v) =
    case ?rigidity $ typeVarName v of
        FreeName -> Just $ varDolanShimWit v
        RigidName -> Nothing
invertNegativeSingular (GroundedDolanSingularType t) = invertNegativeGrounded t
invertNegativeSingular (RecursiveDolanSingularType var t) = do
    t' <- invertNegativeType t
    return $ shimWitToDolan $ recursiveDolanShimWit var t'

invertNegativeType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, ?rigidity :: String -> NameRigidity) =>
    DolanType ground 'Positive a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) 'Negative a)
invertNegativeType (ConsDolanType t NilDolanType) = do
    tf <- invertNegativeSingular t
    return $ mapNegShimWit iJoinR1 tf
invertNegativeType _ = Nothing

invertPolarType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity a.
    ( IsDolanSubtypeGroundType ground
    , SubstitutablePolyShim pshim
    , Is PolarityType polarity
    , ?rigidity :: String -> NameRigidity
    ) =>
    DolanType ground (InvertPolarity polarity) a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) polarity a)
invertPolarType =
    case polarityType @polarity of
        PositiveType -> invertPositiveType @ground
        NegativeType -> invertNegativeType @ground

invertTypeMaybe ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    (String -> NameRigidity) ->
    DolanType ground (InvertPolarity polarity) a ->
    Maybe (PShimWit (pshim Type) (DolanType ground) polarity a)
invertTypeMaybe rigidity = let
    ?rigidity = rigidity
    in invertPolarType @ground

invertTypeM ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    (String -> NameRigidity) ->
    DolanType ground (InvertPolarity polarity) a ->
    TypeResult ground (PShimWit (pshim Type) (DolanType ground) polarity a)
invertTypeM rigidity t =
    case invertTypeMaybe rigidity t of
        Just r -> return r
        Nothing -> withInvertPolarity @polarity $ throw $ UninvertibleTypeError t

invertType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity a.
    (IsDolanSubtypeGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    DolanType ground (InvertPolarity polarity) a ->
    DolanM ground (PShimWit (pshim Type) (DolanType ground) polarity a)
invertType t = runTypeResult $ invertTypeM (\_ -> RigidName) t
