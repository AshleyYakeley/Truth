{-# OPTIONS -fno-warn-orphans #-}

module Data.Shim.PolyIso where

import Data.Shim.CatRange
import Data.Shim.JoinMeet
import Data.Shim.PolarJoinMeet
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyMap
import Data.Shim.PolyShim
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

polarPolyIsoPositive ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pshim k) polarity a b
    -> pshim k a b
polarPolyIsoPositive (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> isoForwards $ unPolyMapT iab
        NegativeType -> isoBackwards $ unPolyMapT iab

polarPolyIsoNegative ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pshim k) polarity a b
    -> pshim k b a
polarPolyIsoNegative (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> isoBackwards $ unPolyMapT iab
        NegativeType -> isoForwards $ unPolyMapT iab

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
       forall (pshim :: PolyShimKind) polarity (a :: Type). (JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => PolarMap (PolyIso pshim Type) polarity (JoinMeetType polarity a (LimitType polarity)) a
polarPolyIsoPolar1 =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT iJoinL1
        NegativeType -> MkPolyMapT iMeetR1

polyIsoForwards :: forall (pshim :: PolyShimKind) k (a :: k) (b :: k). PolyIso pshim k a b -> pshim k a b
polyIsoForwards iab = isoForwards $ unPolyMapT iab

polarPolyIso ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). (Is PolarityType polarity, Category (pshim k))
    => Isomorphism (pshim k) a b
    -> PolarMap (PolyIso pshim k) polarity a b
polarPolyIso iab =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT iab
        NegativeType -> MkPolyMapT $ invert iab
