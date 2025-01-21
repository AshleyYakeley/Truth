{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Bisubstitute
    ( Bisubstitution (..)
    , mkPolarBisubstitution
    , bisubstituteType
    , bisubstituteFlipType
    , bothBisubstitute
    , singleBisubstitute
    , bisubstitutesType
    , funcBisubstituteType
    , bisubstitute
    , bisubstitutes
    , recursiveDolanShimWit
    , mapDolanSingularType
    , mapDolanSingularTypeM
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute.Bisubstitution
import Language.Expression.Dolan.Bisubstitute.Deferred
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Shim
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

mkPolarBisubstitution ::
    forall (ground :: GroundTypeKind) (shim :: ShimKind Type) polarity m tv.
    Is PolarityType polarity =>
    TypeVarT tv ->
    m (PShimWit shim (DolanType ground) polarity tv) ->
    m (PShimWit shim (DolanType ground) (InvertPolarity polarity) tv) ->
    Bisubstitution ground shim m
mkPolarBisubstitution n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution n a b
        NegativeType -> MkBisubstitution n b a

bothBisubstitute ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv t.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    TypeVarT tv ->
    PShimWit (pshim Type) (DolanType ground) polarity tv ->
    PShimWit (pshim Type) (DolanType ground) (InvertPolarity polarity) tv ->
    PShimWit (pshim Type) (DolanType ground) polarity t ->
    PShimWit (pshim Type) (DolanType ground) polarity t
bothBisubstitute var wa wb (MkShimWit tt conv) = let
    bisub :: Bisubstitution ground (pshim Type) Identity
    bisub = withInvertPolarity @polarity $ mkPolarBisubstitution var (pure wa) (pure wb)
    in mapPolarShimWit conv $ runIdentity $ bisubstituteType bisub tt

singleBisubstitute ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv t.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    TypeVarT tv ->
    PShimWit (pshim Type) (DolanType ground) polarity tv ->
    PShimWit (pshim Type) (DolanType ground) polarity t ->
    PShimWit (pshim Type) (DolanType ground) polarity t
singleBisubstitute var wa = withInvertPolarity @polarity $ bothBisubstitute var wa (varDolanShimWit var)

bisubstitutesType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m polarity t.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, MonadInner m, Is PolarityType polarity) =>
    [Bisubstitution ground (pshim Type) m] ->
    DolanType ground polarity t ->
    m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstitutesType [] t = return $ mkPolarShimWit t
bisubstitutesType (sub : subs) t = do
    tf <- bisubstituteType sub t
    chainPolarShimWitM (bisubstitutesType subs) tf

bisubstitute ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m a.
    ( IsDolanGroundType ground
    , SubstitutablePolyShim pshim
    , MonadInner m
    , PShimWitMappable (pshim Type) (DolanType ground) a
    ) =>
    Bisubstitution ground (pshim Type) m ->
    EndoM m a
bisubstitute sub = mapPShimWitsM @_ @(pshim Type) (bisubstituteType sub) (bisubstituteType sub)

bisubstitutes ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m a.
    ( IsDolanGroundType ground
    , SubstitutablePolyShim pshim
    , MonadInner m
    , PShimWitMappable (pshim Type) (DolanType ground) a
    ) =>
    [Bisubstitution ground (pshim Type) m] ->
    EndoM m a
bisubstitutes = concatmap bisubstitute

mapDolanSingularType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    ( forall polarity' t'.
      Is PolarityType polarity' =>
      DolanType ground polarity' t' -> PShimWit (pshim Type) (DolanType ground) polarity' t'
    ) ->
    DolanSingularType ground polarity t ->
    PShimWit (pshim Type) (DolanSingularType ground) polarity t
mapDolanSingularType ff t = runIdentity $ mapDolanSingularTypeM (\t' -> Identity $ ff t') t

bisubstituteFlipType ::
    forall (ground :: GroundTypeKind) polarity m (pshim :: PolyShimKind) a.
    (IsDolanGroundType ground, Is PolarityType polarity, MonadInner m, SubstitutablePolyShim pshim) =>
    Bisubstitution ground (pshim Type) m ->
    FlipType ground polarity a ->
    m (PShimWit (pshim Type) (FlipType ground) polarity a)
bisubstituteFlipType bisub (NormalFlipType t) = do
    MkShimWit t' conv <- bisubstituteType bisub t
    return $ MkShimWit (NormalFlipType t') conv
bisubstituteFlipType _bisub (InvertFlipType t) = return $ mkShimWit $ InvertFlipType t
