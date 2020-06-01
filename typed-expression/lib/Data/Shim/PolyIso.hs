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

type PolyIso :: PolyMapKind -> PolyMapKind
type PolyIso = PolyMapT Isomorphism

instance forall k (pmap :: PolyMapKind). InCategory (pmap k) => InCategory (PolyIso pmap k) where
    cid = MkPolyMapT cid
    MkPolyMapT p <.> MkPolyMapT q = MkPolyMapT $ p <.> q

instance forall k (pmap :: PolyMapKind). InGroupoid (pmap k) => InGroupoid (PolyIso pmap k) where
    cinvert (MkPolyMapT p) = MkPolyMapT $ cinvert p

instance forall k (pmap :: PolyMapKind). Category (pmap k) => Category (PolyIso pmap k) where
    id = MkPolyMapT id
    MkPolyMapT p . MkPolyMapT q = MkPolyMapT $ p . q

instance forall k (pmap :: PolyMapKind). Groupoid (pmap k) => Groupoid (PolyIso pmap k) where
    invert (MkPolyMapT p) = MkPolyMapT $ invert p

polarPolyIsoForwards ::
       forall (pmap :: PolyMapKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pmap k) polarity a b
    -> PolarMap (pmap k) polarity a b
polarPolyIsoForwards (MkPolarMap iab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> isoForwards $ unPolyMapT iab
        NegativeType -> isoForwards $ unPolyMapT iab

polarPolyIsoSingle ::
       forall (pmap :: PolyMapKind) polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap (PolyIso pmap k) polarity a b
    -> pmap k a b
polarPolyIsoSingle (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> isoForwards $ unPolyMapT iab
        NegativeType -> isoBackwards $ unPolyMapT iab

instance forall (pmap :: PolyMapKind). ApplyPolyShim pmap => ApplyPolyShim (PolyIso pmap) where
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
       forall (pmap :: PolyMapKind) polarity (a :: Type). (JoinMeetCategory (pmap Type), Is PolarityType polarity)
    => PolarMap (PolyIso pmap Type) polarity (JoinMeetType polarity a (LimitType polarity)) a
polarPolyIsoPolar1 =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT bijoin1
        NegativeType -> MkPolyMapT bimeet1

polarPolyIsoBimap ::
       forall (pmap :: PolyMapKind) polarity a1 a2 b1 b2. (JoinMeetCategory (pmap Type), Is PolarityType polarity)
    => PolarMap (PolyIso pmap Type) polarity a1 b1
    -> PolarMap (PolyIso pmap Type) polarity a2 b2
    -> PolarMap (PolyIso pmap Type) polarity (JoinMeetType polarity a1 a2) (JoinMeetType polarity b1 b2)
polarPolyIsoBimap =
    case polarityType @polarity of
        PositiveType ->
            \(MkPolarMap (MkPolyMapT m1)) (MkPolarMap (MkPolyMapT m2)) -> MkPolarMap $ MkPolyMapT $ biJoinBimap m1 m2
        NegativeType ->
            \(MkPolarMap (MkPolyMapT m1)) (MkPolarMap (MkPolyMapT m2)) -> MkPolarMap $ MkPolyMapT $ biMeetBimap m1 m2
