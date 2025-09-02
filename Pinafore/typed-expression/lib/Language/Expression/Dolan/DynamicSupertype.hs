module Language.Expression.Dolan.DynamicSupertype
    ( DolanExprShim
    , PolyGreatestDynamicSupertype (..)
    , nullPolyGreatestDynamicSupertype
    , simplePolyGreatestDynamicSupertype
    , runPolyGreatestDynamicSupertype
    , PolyGDSArgs (..)
    , getPolyGreatestDynamicSupertype
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type DolanExprShim (ground :: GroundTypeKind) = PolyComposeShim (DolanOpenExpression ground) (DolanPolyShim ground) Type

type PolyGreatestDynamicSupertype :: GroundTypeKind -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data PolyGreatestDynamicSupertype ground dv gt where
    NullPolyGreatestDynamicSupertype ::
        forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
        PolyGreatestDynamicSupertype ground dv gt
    MkPolyGreatestDynamicSupertype ::
        forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) t.
        NonpolarArguments TypeVarT dv gt t ->
        PShimWit (DolanExprShim ground) (DolanGroundedType ground) 'Negative (Maybe t) ->
        PolyGreatestDynamicSupertype ground dv gt

nullPolyGreatestDynamicSupertype ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    PolyGreatestDynamicSupertype ground dv gt
nullPolyGreatestDynamicSupertype = NullPolyGreatestDynamicSupertype

simplePolyGreatestDynamicSupertype ::
    forall (ground :: GroundTypeKind) (pt :: Type) (t :: Type).
    ground '[] pt ->
    DolanShim ground pt (Maybe t) ->
    PolyGreatestDynamicSupertype ground '[] t
simplePolyGreatestDynamicSupertype wt conv =
    MkPolyGreatestDynamicSupertype NilCCRArguments
        $ MkShimWit (MkDolanGroundedType wt NilCCRArguments)
        $ MkPolarShim
        $ purePolyComposeShim conv

assignArgumentBisubstitutions ::
    forall
        (w :: Polarity -> Type -> Type)
        (shim :: ShimKind Type)
        (sv :: CCRVariance)
        ta
        tb.
    Category shim =>
    NonpolarArgument TypeVarT sv ta ->
    CCRPolarArgument w 'Negative sv tb ->
    (ta :~: tb, [Bisubstitution w shim Maybe])
assignArgumentBisubstitutions (CoNonpolarArgument var) (CoCCRPolarArgument tg) =
    assignTypeVarWit var tg
        $ (Refl, [MkBisubstitution var Nothing (Just $ mkShimWit tg)])
assignArgumentBisubstitutions (ContraNonpolarArgument var) (ContraCCRPolarArgument tg) =
    assignTypeVarWit var tg
        $ (Refl, [MkBisubstitution var (Just $ mkShimWit tg) Nothing])
assignArgumentBisubstitutions (RangeNonpolarArgument varp varq) (RangeCCRPolarArgument tp tq) =
    assignTypeVarWit varp tp
        $ assignTypeVarWit varq tq
        $ ( Refl
          ,
              [ MkBisubstitution varp (Just $ mkShimWit tp) Nothing
              , MkBisubstitution varq Nothing (Just $ mkShimWit tq)
              ]
          )

assignArgumentsBisubstitutions ::
    forall
        (w :: Polarity -> Type -> Type)
        (shim :: ShimKind Type)
        (dv :: CCRVariances)
        (gt :: CCRVariancesKind dv)
        ta
        tb.
    Category shim =>
    NonpolarArguments TypeVarT dv gt ta ->
    CCRPolarArguments dv w gt 'Negative tb ->
    (ta :~: tb, [Bisubstitution w shim Maybe])
assignArgumentsBisubstitutions NilCCRArguments NilCCRArguments = (Refl, [])
assignArgumentsBisubstitutions (ConsCCRArguments varg vargs) (ConsCCRArguments targ targs) =
    case assignArgumentBisubstitutions @w @shim varg targ of
        (Refl, bisubs1) ->
            case assignArgumentsBisubstitutions @w @shim vargs targs of
                (Refl, bisubsr) ->
                    (Refl, bisubs1 <> bisubsr)

type PolyGDSArgs :: GroundTypeKind -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data PolyGDSArgs (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) where
    MkPolyGDSArgs ::
        forall
            (ground :: GroundTypeKind)
            (dv :: CCRVariances)
            (gt :: CCRVariancesKind dv)
            (dv' :: CCRVariances)
            (gt' :: CCRVariancesKind dv').
        ground dv' gt' ->
        ( forall t.
          CCRPolarArguments dv (DolanType ground) gt 'Negative t ->
          Maybe (PolarShimWit (DolanExprShim ground) (CCRPolarArguments dv' (DolanType ground) gt' 'Negative) 'Negative (Maybe t))
        ) ->
        PolyGDSArgs ground dv gt

getPolyGreatestDynamicSupertype ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    IsDolanGroundType ground =>
    PolyGreatestDynamicSupertype ground dv gt ->
    Maybe (PolyGDSArgs ground dv gt)
getPolyGreatestDynamicSupertype = \case
    NullPolyGreatestDynamicSupertype -> Nothing
    MkPolyGreatestDynamicSupertype varargs ps@(MkShimWit (MkDolanGroundedType gt _) _) -> Just
        $ MkPolyGDSArgs gt
        $ \args -> case assignArgumentsBisubstitutions @(DolanType ground) @(DolanExprShim ground) varargs args of
            (Refl, bisubs) -> do
                MkShimWit (MkDolanGroundedType gt' gargs') conv <- unEndoM (bisubstitutesGrounded bisubs) ps
                (Refl, HRefl) <- groundTypeTestEquality gt gt'
                pure $ MkShimWit gargs' conv

runPolyGreatestDynamicSupertype ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) t.
    IsDolanGroundType ground =>
    PolyGreatestDynamicSupertype ground dv gt ->
    CCRPolarArguments dv (DolanType ground) gt 'Negative t ->
    Maybe (PShimWit (DolanExprShim ground) (DolanGroundedType ground) 'Negative (Maybe t))
runPolyGreatestDynamicSupertype pgds args = do
    MkPolyGDSArgs gt f <- getPolyGreatestDynamicSupertype pgds
    MkShimWit gargs conv <- f args
    pure $ MkShimWit (MkDolanGroundedType gt gargs) conv
