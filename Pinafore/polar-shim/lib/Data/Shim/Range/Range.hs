module Data.Shim.Range.Range where

import Data.Shim.Mono
import Shapes

type Contra :: (kp, kq) -> kp
type Contra pq = Fst pq

type Co :: (kp, kq) -> kq
type Co pq = Snd pq

-- | For dealing with non-co/contravariance, see Dolan sec. 9.1
data Range (shim :: ShimKind Type) (t :: Type) (pq :: (Type, Type)) =
    MkRange (shim (Contra pq) t)
            (shim t (Co pq))

rangeCo :: Range shim t '( p, q) -> shim t q
rangeCo (MkRange _ tq) = tq

rangeContra :: Range shim t '( p, q) -> shim p t
rangeContra (MkRange pt _) = pt

identityRange :: Category shim => Range shim t '( t, t)
identityRange = MkRange id id

coMapRange :: Category shim => shim q1 q2 -> Range shim t '( p, q1) -> Range shim t '( p, q2)
coMapRange qq (MkRange p q) = MkRange p (qq . q)

contraMapRange :: Category shim => shim p2 p1 -> Range shim t '( p1, q) -> Range shim t '( p2, q)
contraMapRange pp (MkRange p q) = MkRange (p . pp) q

pairRange ::
       CartesianShim shim
    => Range shim a '( ap, aq)
    -> Range shim b '( bp, bq)
    -> Range shim (a, b) '( (ap, bp), (aq, bq))
pairRange (MkRange pa aq) (MkRange pb bq) = MkRange (pairShim pa pb) (pairShim aq bq)

eitherRange ::
       CartesianShim shim
    => Range shim a '( ap, aq)
    -> Range shim b '( bp, bq)
    -> Range shim (Either a b) '( Either ap bp, Either aq bq)
eitherRange (MkRange pa aq) (MkRange pb bq) = MkRange (eitherShim pa pb) (eitherShim aq bq)

coRange :: JoinMeetShim shim => shim t q -> Range shim t '( t, q)
coRange tq = MkRange id tq

contraRange :: JoinMeetShim shim => shim p t -> Range shim t '( p, t)
contraRange pt = MkRange pt id

rangeBijection :: Range shim a '( b, b) -> Isomorphism shim a b
rangeBijection (MkRange ba ab) = MkIsomorphism ab ba

bijectionRange :: Isomorphism shim a b -> Range shim a '( b, b)
bijectionRange (MkIsomorphism ab ba) = MkRange ba ab

bijectRanges :: Category shim => Range shim a '( p, q) -> Range shim b '( q, p) -> Isomorphism shim a b
bijectRanges (MkRange pa aq) (MkRange qb bp) = MkIsomorphism (qb . aq) (pa . bp)
