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

polarPolyIsoPositive ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pshim k) polarity a b
    -> pshim k a b
polarPolyIsoPositive (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> isoForwards $ unPolyMapT iab
        NegativeType -> isoBackwards $ unPolyMapT iab

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

instance forall (pshim :: PolyShimKind). ApplyPolyShim pshim => ApplyPolyShim (PolySemiIso pshim) where
    applyPolyShim CovarianceType (MkPolyMapT (MkSemiIsomorphism fab mfba)) (MkPolyMapT (MkSemiIsomorphism xab mxba)) =
        MkPolyMapT $
        MkSemiIsomorphism (applyPolyShim CovarianceType fab xab) (liftA2 (applyPolyShim CovarianceType) mfba mxba)
    applyPolyShim ContravarianceType (MkPolyMapT (MkSemiIsomorphism fab mfba)) (MkCatDual (MkPolyMapT (MkSemiIsomorphism xab mxba))) =
        MkPolyMapT $
        MkSemiIsomorphism
            (applyPolyShim ContravarianceType fab $ MkCatDual xab)
            ((\fba xba -> applyPolyShim ContravarianceType fba $ MkCatDual xba) <$> mfba <*> mxba)
    applyPolyShim RangevarianceType (MkPolyMapT (MkSemiIsomorphism fab mfba)) (MkCatRange (MkPolyMapT (MkSemiIsomorphism xab1 mxba1)) (MkPolyMapT (MkSemiIsomorphism xab2 mxba2))) =
        MkPolyMapT $
        MkSemiIsomorphism
            (applyPolyShim RangevarianceType fab (MkCatRange xab1 xab2))
            ((\fba xba1 xba2 -> applyPolyShim RangevarianceType fba (MkCatRange xba1 xba2)) <$> mfba <*> mxba1 <*> mxba2)

polarPolyIsoPolar1 ::
       forall (pshim :: PolyShimKind) polarity (a :: Type). (JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => PolarMap (PolyIso pshim Type) polarity (JoinMeetType polarity a (LimitType polarity)) a
polarPolyIsoPolar1 =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT iJoinL1
        NegativeType -> MkPolyMapT iMeetR1

type PolySemiIso :: PolyShimKind -> PolyShimKind
type PolySemiIso = PolyMapT SemiIsomorphism

polySemiIsoForwards ::
       forall (pshim :: PolyShimKind) k (a :: k) (b :: k). PolyMapT SemiIsomorphism pshim k a b -> pshim k a b
polySemiIsoForwards iab = semiIsoForwards $ unPolyMapT iab

polarPolySemiIsoForwards ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolySemiIso pshim k) polarity a b
    -> PolarMap (pshim k) polarity a b
polarPolySemiIsoForwards (MkPolarMap iab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> polySemiIsoForwards iab
        NegativeType -> polySemiIsoForwards iab

polarPolySemiIsoBackwards ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolySemiIso pshim k) polarity a b
    -> Maybe (PolarMap (pshim k) polarity b a)
polarPolySemiIsoBackwards (MkPolarMap iab) =
    fmap MkPolarMap $
    case polarityType @polarity of
        PositiveType -> semiIsoBackwards $ unPolyMapT iab
        NegativeType -> semiIsoBackwards $ unPolyMapT iab

polarPolySemiIsoPositive ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolySemiIso pshim k) polarity a b
    -> Maybe (pshim k a b)
polarPolySemiIsoPositive (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> Just $ semiIsoForwards $ unPolyMapT iab
        NegativeType -> semiIsoBackwards $ unPolyMapT iab

polarPolySemiIsoNegative ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolySemiIso pshim k) polarity a b
    -> Maybe (pshim k b a)
polarPolySemiIsoNegative (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> semiIsoBackwards $ unPolyMapT iab
        NegativeType -> Just $ semiIsoForwards $ unPolyMapT iab

polarPolySemiIsoInvert ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolySemiIso pshim k) polarity a b
    -> Maybe (PolarMap (PolySemiIso pshim k) polarity b a)
polarPolySemiIsoInvert (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> fmap (MkPolarMap . MkPolyMapT) $ semiIsoInvert $ unPolyMapT iab
        NegativeType -> fmap (MkPolarMap . MkPolyMapT) $ semiIsoInvert $ unPolyMapT iab

polarPolySemiIsoUnpolarPositive ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolySemiIso pshim k) polarity a b
    -> Maybe (PolySemiIso pshim k a b)
polarPolySemiIsoUnpolarPositive (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> Just $ iab
        NegativeType -> fmap MkPolyMapT $ semiIsoInvert $ unPolyMapT iab

polarPolySemiIsoUnpolarNegative ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolySemiIso pshim k) polarity a b
    -> Maybe (PolySemiIso pshim k b a)
polarPolySemiIsoUnpolarNegative (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> fmap MkPolyMapT $ semiIsoInvert $ unPolyMapT iab
        NegativeType -> Just $ iab

polarPolySemiIsoBimap ::
       forall (pshim :: PolyShimKind) polarity a1 a2 b1 b2. (JoinMeetCategory (pshim Type), Is PolarityType polarity)
    => PolarMap (PolySemiIso pshim Type) polarity a1 b1
    -> PolarMap (PolySemiIso pshim Type) polarity a2 b2
    -> PolarMap (PolySemiIso pshim Type) polarity (JoinMeetType polarity a1 a2) (JoinMeetType polarity b1 b2)
polarPolySemiIsoBimap =
    case polarityType @polarity of
        PositiveType ->
            \(MkPolarMap (MkPolyMapT m1)) (MkPolarMap (MkPolyMapT m2)) -> MkPolarMap $ MkPolyMapT $ iJoinPair m1 m2
        NegativeType ->
            \(MkPolarMap (MkPolyMapT m1)) (MkPolarMap (MkPolyMapT m2)) -> MkPolarMap $ MkPolyMapT $ iMeetPair m1 m2

polarPolyToSemiIso ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (pshim k) polarity a b
    -> PolarMap (PolySemiIso pshim k) polarity a b
polarPolyToSemiIso (MkPolarMap iab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT $ semiIso iab
        NegativeType -> MkPolyMapT $ semiIso iab

polarPolyFromSemiIsoShimWit ::
       forall (pshim :: PolyShimKind) polarity w t. Is PolarityType polarity
    => ShimWit (PolySemiIso pshim Type) w polarity t
    -> ShimWit (pshim Type) w polarity t
polarPolyFromSemiIsoShimWit (MkShimWit t conv) = MkShimWit t $ polarPolySemiIsoForwards conv

polarPolyToSemiIsoShimWit ::
       forall (pshim :: PolyShimKind) polarity w t. Is PolarityType polarity
    => ShimWit (pshim Type) w polarity t
    -> ShimWit (PolySemiIso pshim Type) w polarity t
polarPolyToSemiIsoShimWit (MkShimWit t conv) = MkShimWit t $ polarPolyToSemiIso conv

lazySemiIso ::
       forall (pshim :: PolyShimKind) (a :: Type) (b :: Type). EnhancedFunction (pshim Type)
    => PolySemiIso pshim Type a b
    -> PolySemiIso pshim Type a b
lazySemiIso (MkPolyMapT ~(MkSemiIsomorphism ab mba)) =
    MkPolyMapT $ MkSemiIsomorphism (lazyEnhanced ab) (fmap lazyEnhanced mba)

lazyPolarSemiIso ::
       forall (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (EnhancedFunction (pshim Type), Is PolarityType polarity)
    => PolarMap (PolySemiIso pshim Type) polarity a b
    -> PolarMap (PolySemiIso pshim Type) polarity a b
lazyPolarSemiIso =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap mab) -> MkPolarMap $ lazySemiIso mab
        NegativeType -> \(MkPolarMap mab) -> MkPolarMap $ lazySemiIso mab
