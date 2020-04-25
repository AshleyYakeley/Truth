module Data.Shim.JMIsoShim where

import Data.Shim.CatRange
import Data.Shim.JMShim
import Data.Shim.Polarity
import Data.Shim.PolyShim
import Data.Shim.Variance
import Shapes

newtype JMIsoShim (a :: k) (b :: k) = MkJMIsoShim
    { unJMIsoShim :: Isomorphism JMShim a b
    }

jmIsoPolarForwards ::
       forall polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMapType JMIsoShim polarity a b
    -> JMShim a b
jmIsoPolarForwards conv =
    case representative @_ @_ @polarity of
        PositiveType -> isoForwards $ unJMIsoShim conv
        NegativeType -> isoBackwards $ unJMIsoShim conv

jmIsoPolarBackwards ::
       forall polarity k (a :: k) (b :: k). Is PolarityType polarity
    => PolarMapType JMIsoShim polarity a b
    -> JMShim b a
jmIsoPolarBackwards conv =
    case representative @_ @_ @polarity of
        PositiveType -> isoBackwards $ unJMIsoShim conv
        NegativeType -> isoForwards $ unJMIsoShim conv

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
