module Data.Shim.Range where

import Data.Shim.JoinMeet
import Shapes

type family Contra (pq :: (Type, Type)) :: Type where
    Contra '( p, q) = p

type family Co (pq :: (Type, Type)) :: Type where
    Co '( p, q) = q

-- | For dealing with non-co/contravariance, see Dolan sec. 9.1
data Range (shim :: Type -> Type -> Type) (t :: Type) (pq :: (Type, Type)) =
    MkRange (shim (Contra pq) t)
            (shim t (Co pq))

rangeCo :: Range shim t '( p, q) -> shim t q
rangeCo (MkRange _ tq) = tq

rangeContra :: Range shim t '( p, q) -> shim p t
rangeContra (MkRange pt _) = pt

identityRange :: InCategory shim => Range shim t '( t, t)
identityRange = MkRange cid cid

coMapRange :: InCategory shim => shim q1 q2 -> Range shim t '( p, q1) -> Range shim t '( p, q2)
coMapRange qq (MkRange p q) = MkRange p (qq <.> q)

contraMapRange :: InCategory shim => shim p2 p1 -> Range shim t '( p1, q) -> Range shim t '( p2, q)
contraMapRange pp (MkRange p q) = MkRange (p <.> pp) q

pairRange :: Shim shim => Range shim a '( ap, aq) -> Range shim b '( bp, bq) -> Range shim (a, b) '( (ap, bp), (aq, bq))
pairRange (MkRange pa aq) (MkRange pb bq) = MkRange (pairShim pa pb) (pairShim aq bq)

eitherRange ::
       Shim shim
    => Range shim a '( ap, aq)
    -> Range shim b '( bp, bq)
    -> Range shim (Either a b) '( Either ap bp, Either aq bq)
eitherRange (MkRange pa aq) (MkRange pb bq) = MkRange (eitherShim pa pb) (eitherShim aq bq)

coRange :: JoinMeetCategory shim => shim t q -> Range shim t '( t, q)
coRange tq = MkRange cid tq

contraRange :: JoinMeetCategory shim => shim p t -> Range shim t '( p, t)
contraRange pt = MkRange pt cid

rangeBijection :: Range shim a '( b, b) -> Isomorphism shim a b
rangeBijection (MkRange ba ab) = MkIsomorphism ab ba

bijectionRange :: (InKind a, InKind b) => Isomorphism shim a b -> Range shim a '( b, b)
bijectionRange (MkIsomorphism ab ba) = MkRange ba ab

bijectRanges :: InCategory shim => Range shim a '( p, q) -> Range shim b '( q, p) -> Isomorphism shim a b
bijectRanges (MkRange pa aq) (MkRange qb bp) = MkIsomorphism (qb <.> aq) (pa <.> bp)
