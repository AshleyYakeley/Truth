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

polarPolyIso ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). (Is PolarityType polarity, Category (pshim k))
    => PolarMap (PolyIso pshim k) polarity a b
    -> Isomorphism (pshim k) a b
polarPolyIso (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> unPolyMapT iab
        NegativeType -> invert $ unPolyMapT iab

isoPolarPoly ::
       forall (pshim :: PolyShimKind) polarity k (a :: k) (b :: k). (Is PolarityType polarity, Category (pshim k))
    => Isomorphism (pshim k) a b
    -> PolarMap (PolyIso pshim k) polarity a b
isoPolarPoly iab =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkPolyMapT iab
        NegativeType -> MkPolyMapT $ invert iab

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
        MkPolyMapT $ MkIsomorphism (applyCoPolyShim fab xab) (applyCoPolyShim fba xba)
    applyPolyShim ContravarianceType (MkPolyMapT (MkIsomorphism fab fba)) (MkCatDual (MkPolyMapT (MkIsomorphism xab xba))) =
        MkPolyMapT $ MkIsomorphism (applyContraPolyShim fab xab) (applyContraPolyShim fba xba)
    applyPolyShim RangevarianceType (MkPolyMapT (MkIsomorphism fab fba)) (MkCatRange (MkPolyMapT (MkIsomorphism xab1 xba1)) (MkPolyMapT (MkIsomorphism xab2 xba2))) =
        MkPolyMapT $ MkIsomorphism (applyRangePolyShim fab xab1 xab2) (applyRangePolyShim fba xba1 xba2)

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

instance forall (pshim :: PolyShimKind) k. (CoercibleKind k, IsoMapShim (pshim k), Category (pshim k)) =>
             IsoMapShim (PolyIso pshim k) where
    isoMapShim ::
           (InKind pa, InKind pb, InKind qa, InKind qb)
        => String
        -> (KindFunction pa pb -> KindFunction qa qb)
        -> (KindFunction pb pa -> KindFunction qb qa)
        -> PolyIso pshim k pa pb
        -> PolyIso pshim k qa qb
    isoMapShim t f1 f2 (MkPolyMapT (MkIsomorphism ab ba)) =
        MkPolyMapT $ MkIsomorphism (isoMapShim t f1 f2 ab) (isoMapShim t f2 f1 ba)

instance forall (pshim :: PolyShimKind) k. (CoercibleKind k, CoerceShim (pshim k), Category (pshim k)) =>
             CoerceShim (PolyIso pshim k) where
    coercionEnhanced n c = MkPolyMapT $ MkIsomorphism (coercionEnhanced n c) (coercionEnhanced n $ invert c)
    enhancedCoercion (MkPolyMapT (MkIsomorphism ab ba)) = enhancedCoercion ab <|> fmap invert (enhancedCoercion ba)

instance forall (pshim :: PolyShimKind). AllInCategory pshim => ReduciblePolyShim (PolyIso pshim) where
    type ReducedPolyShim (PolyIso pshim) = PolyIso pshim
