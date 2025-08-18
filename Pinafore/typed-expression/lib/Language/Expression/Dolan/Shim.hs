module Language.Expression.Dolan.Shim where

import Data.Shim
import Shapes

class
    (JoinMeetIsoShim (pshim Type), IsoMapShim (pshim Type), CCRVariancesShim pshim, ReduciblePolyShim pshim) =>
    SubstitutablePolyShim (pshim :: PolyShimKind)
    where
    reducedSubstitutablePolyShim ::
        Dict
            ( SubstitutablePolyShim (ReducedPolyShim pshim)
            , LazyShim (ReducedPolyShim pshim Type)
            , ReducedPolyShim (ReducedPolyShim pshim) Type ~ ReducedPolyShim pshim Type
            )

instance SubstitutablePolyShim NullShim where
    reducedSubstitutablePolyShim = Dict

instance
    forall m (pshim :: PolyShimKind).
    (Applicative m, SubstitutablePolyShim pshim) =>
    SubstitutablePolyShim (PolyComposeShim m pshim)
    where
    reducedSubstitutablePolyShim =
        case reducedSubstitutablePolyShim @pshim of
            Dict -> Dict

instance
    forall (pshim :: PolyShimKind).
    (SubstitutablePolyShim pshim, LazyShim (pshim Type)) =>
    SubstitutablePolyShim (PolyDual pshim)
    where
    reducedSubstitutablePolyShim =
        case reducedSubstitutablePolyShim @pshim of
            Dict -> Dict

instance
    forall (pshim :: PolyShimKind).
    (SubstitutablePolyShim pshim, LazyShim (pshim Type)) =>
    SubstitutablePolyShim (PolyIso pshim)
    where
    reducedSubstitutablePolyShim =
        case reducedSubstitutablePolyShim @pshim of
            Dict -> Dict

instance SubstitutablePolyShim JMShim where
    reducedSubstitutablePolyShim = Dict

class (SubstitutablePolyShim pshim, Groupoid (pshim Type)) => SubstitutableIsoPolyShim (pshim :: PolyShimKind) where
    reducedSubstitutableIsoPolyShim ::
        Dict
            ( SubstitutableIsoPolyShim (ReducedPolyShim pshim)
            , LazyShim (ReducedPolyShim pshim Type)
            , Groupoid (ReducedPolyShim pshim Type)
            )

instance
    forall m (pshim :: PolyShimKind).
    (Applicative m, SubstitutableIsoPolyShim pshim) =>
    SubstitutableIsoPolyShim (PolyComposeShim m pshim)
    where
    reducedSubstitutableIsoPolyShim =
        case reducedSubstitutableIsoPolyShim @pshim of
            Dict -> Dict

instance
    forall (pshim :: PolyShimKind).
    (SubstitutableIsoPolyShim pshim, LazyShim (pshim Type)) =>
    SubstitutableIsoPolyShim (PolyDual pshim)
    where
    reducedSubstitutableIsoPolyShim =
        case reducedSubstitutableIsoPolyShim @pshim of
            Dict -> Dict

instance
    forall (pshim :: PolyShimKind).
    (SubstitutablePolyShim pshim, LazyShim (pshim Type)) =>
    SubstitutableIsoPolyShim (PolyIso pshim)
    where
    reducedSubstitutableIsoPolyShim =
        case reducedSubstitutablePolyShim @pshim of
            Dict -> Dict

type IsDolanPolyShim :: PolyShimKind -> Constraint
type IsDolanPolyShim pshim =
    ( SubstitutablePolyShim pshim
    , JoinMeetShim (pshim Type)
    , LazyShim (pshim Type)
    , CartesianShim (pshim Type)
    , ToFunctionShim (pshim Type)
    )
