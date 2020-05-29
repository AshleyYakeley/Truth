module Data.Shim.JMIsoShim where

import Data.Shim.CatRange
import Data.Shim.JMShim
import Data.Shim.JoinMeet
import Data.Shim.PolarJoinMeet
import Data.Shim.PolarMap
import Data.Shim.Polarity
import Data.Shim.PolyShim
import Data.Shim.Variance
import Shapes

-- | Necessary because we need a type of kind `forall k. k -> k -> Type`.
-- FIXME: getting rid of this requires GHC 8.10, use kind "forall k -> k -> k -> Type" instead
newtype JMIsoShim (a :: k) (b :: k) = MkJMIsoShim
    { unJMIsoShim :: Isomorphism JMShim a b
    }

jmIsoForwards ::
       forall polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap JMIsoShim polarity a b
    -> PolarMap JMShim polarity a b
jmIsoForwards (MkPolarMap iab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> isoForwards $ unJMIsoShim iab
        NegativeType -> isoForwards $ unJMIsoShim iab

jmIsoSingle ::
       forall polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap JMIsoShim polarity a b
    -> JMShim a b
jmIsoSingle (MkPolarMap iab) =
    case polarityType @polarity of
        PositiveType -> isoForwards $ unJMIsoShim iab
        NegativeType -> isoBackwards $ unJMIsoShim iab

instance CoercibleKind k => InCategory (JMIsoShim :: k -> k -> Type) where
    cid = MkJMIsoShim cid
    MkJMIsoShim p <.> MkJMIsoShim q = MkJMIsoShim $ p <.> q

instance CoercibleKind k => InGroupoid (JMIsoShim :: k -> k -> Type) where
    cinvert (MkJMIsoShim p) = MkJMIsoShim $ cinvert p

instance Category (JMIsoShim :: Type -> Type -> Type) where
    id = cid
    (.) = (<.>)

instance Groupoid (JMIsoShim :: Type -> Type -> Type) where
    invert = cinvert

instance ConPolyShim JMIsoShim where
    consShimFunc CovarianceType (MkJMIsoShim (MkIsomorphism fab fba)) (MkJMIsoShim (MkIsomorphism xab xba)) =
        MkJMIsoShim $ MkIsomorphism (consShimFunc CovarianceType fab xab) (consShimFunc CovarianceType fba xba)
    consShimFunc ContravarianceType (MkJMIsoShim (MkIsomorphism fab fba)) (MkCatDual (MkJMIsoShim (MkIsomorphism xab xba))) =
        MkJMIsoShim $
        MkIsomorphism
            (consShimFunc ContravarianceType fab $ MkCatDual xab)
            (consShimFunc ContravarianceType fba $ MkCatDual xba)
    consShimFunc RangevarianceType (MkJMIsoShim (MkIsomorphism fab fba)) (MkCatRange (MkJMIsoShim (MkIsomorphism xab1 xba1)) (MkJMIsoShim (MkIsomorphism xab2 xba2))) =
        MkJMIsoShim $
        MkIsomorphism
            (consShimFunc RangevarianceType fab (MkCatRange xab1 xab2))
            (consShimFunc RangevarianceType fba (MkCatRange xba1 xba2))

jmIsoPolar1 ::
       forall polarity a. Is PolarityType polarity
    => PolarMap JMIsoShim polarity (JoinMeetType polarity a (LimitType polarity)) a
jmIsoPolar1 =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> MkJMIsoShim bijoin1
        NegativeType -> MkJMIsoShim bimeet1

jmIsoBimap ::
       forall polarity a1 a2 b1 b2. Is PolarityType polarity
    => PolarMap JMIsoShim polarity a1 b1
    -> PolarMap JMIsoShim polarity a2 b2
    -> PolarMap JMIsoShim polarity (JoinMeetType polarity a1 a2) (JoinMeetType polarity b1 b2)
jmIsoBimap =
    case polarityType @polarity of
        PositiveType ->
            \(MkPolarMap (MkJMIsoShim m1)) (MkPolarMap (MkJMIsoShim m2)) -> MkPolarMap $ MkJMIsoShim $ biJoinBimap m1 m2
        NegativeType ->
            \(MkPolarMap (MkJMIsoShim m1)) (MkPolarMap (MkJMIsoShim m2)) -> MkPolarMap $ MkJMIsoShim $ biMeetBimap m1 m2
