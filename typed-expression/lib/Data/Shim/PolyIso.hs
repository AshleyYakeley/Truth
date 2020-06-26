{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.PolyIso where

import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.PolarJoinMeet
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyMap
import Data.Shim.PolyShim
import Data.Shim.ShimWit
import Data.Shim.Variance
import Shapes

type PolyIso :: PolyShimKind -> PolyShimKind
type PolyIso = PolyMapT Isomorphism

polarPolyIsoForwards ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pshim k) polarity a b
    -> PolarMap (pshim k) polarity a b
polarPolyIsoForwards (MkPolarMap iab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> isoForwards $ unPolyMapT iab
        NegativeType -> isoForwards $ unPolyMapT iab

polarPolyIsoBackwards ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pshim k) polarity a b
    -> PolarMap (pshim k) polarity b a
polarPolyIsoBackwards (MkPolarMap iab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> isoBackwards $ unPolyMapT iab
        NegativeType -> isoBackwards $ unPolyMapT iab

mkPolarPolyIso ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (pshim k) polarity a b
    -> PolarMap (pshim k) polarity b a
    -> PolarMap (PolyIso pshim k) polarity a b
mkPolarPolyIso (MkPolarMap isoForwards) (MkPolarMap isoBackwards) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT $ MkIsomorphism {..}
        NegativeType -> MkPolyMapT $ MkIsomorphism {..}

polarPolyIsoSingle ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pshim k) polarity a b
    -> pshim k a b
polarPolyIsoSingle (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> isoForwards $ unPolyMapT iab
        NegativeType -> isoBackwards $ unPolyMapT iab

isoPolyIso ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k).
       (Is PolarityType polarity, InCategory (pshim k), InKind a, InKind b)
    => Isomorphism (pshim k) a b
    -> PolarMap (PolyIso pshim k) polarity a b
isoPolyIso iso =
    case polarityType @polarity of
        PositiveType -> MkPolarMap $ MkPolyMapT iso
        NegativeType -> MkPolarMap $ MkPolyMapT $ cinvert iso

instance forall (pshim :: PolyShimKind). ApplyPolyShim pshim => ApplyPolyShim (PolyIso pshim) where
    applyPolyShim CovarianceType (MkPolyMapT (MkIsomorphism fab fba)) (MkPolyMapT (MkIsomorphism xab xba)) =
        MkPolyMapT $ MkIsomorphism (applyPolyShim CovarianceType fab xab) (applyPolyShim CovarianceType fba xba)
    applyPolyShim ContravarianceType (MkPolyMapT (MkIsomorphism fab fba)) (MkCatDual (MkPolyMapT (MkIsomorphism xab xba))) =
        MkPolyMapT $
        MkIsomorphism
            (applyPolyShim ContravarianceType fab $ MkCatDual xab)
            (applyPolyShim ContravarianceType fba $ MkCatDual xba)
    applyPolyShim RangevarianceType (MkPolyMapT (MkIsomorphism fab fba)) (MkCatRange (MkPolyMapT (MkIsomorphism xab1 xba1)) (MkPolyMapT (MkIsomorphism xab2 xba2))) =
        MkPolyMapT $
        MkIsomorphism
            (applyPolyShim RangevarianceType fab (MkCatRange xab1 xab2))
            (applyPolyShim RangevarianceType fba (MkCatRange xba1 xba2))

polarPolyIsoPolar1 ::
       forall (pshim :: PolyShimKind) polarity (a :: Type). (JoinMeetCategory (pshim Type), Is PolarityType polarity)
    => PolarMap (PolyIso pshim Type) polarity (JoinMeetType polarity a (LimitType polarity)) a
polarPolyIsoPolar1 =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT iJoinL1
        NegativeType -> MkPolyMapT iMeetR1

polarPolyIsoBimap ::
       forall (pshim :: PolyShimKind) polarity a1 a2 b1 b2. (JoinMeetCategory (pshim Type), Is PolarityType polarity)
    => PolarMap (PolyIso pshim Type) polarity a1 b1
    -> PolarMap (PolyIso pshim Type) polarity a2 b2
    -> PolarMap (PolyIso pshim Type) polarity (JoinMeetType polarity a1 a2) (JoinMeetType polarity b1 b2)
polarPolyIsoBimap =
    case polarityType @polarity of
        PositiveType ->
            \(MkPolarMap (MkPolyMapT m1)) (MkPolarMap (MkPolyMapT m2)) -> MkPolarMap $ MkPolyMapT $ iJoinPair m1 m2
        NegativeType ->
            \(MkPolarMap (MkPolyMapT m1)) (MkPolarMap (MkPolyMapT m2)) -> MkPolarMap $ MkPolyMapT $ iMeetPair m1 m2

polarPolyIsoShimWit ::
       forall (pshim :: PolyShimKind) polarity w t. Is PolarityType polarity
    => ShimWit (PolyIso pshim Type) w polarity t
    -> ShimWit (pshim Type) w polarity t
polarPolyIsoShimWit (MkShimWit t conv) = MkShimWit t $ polarPolyIsoForwards conv
