module Data.Shim.Range where

import Data.Shim.JoinMeet
import Data.Shim.Polarity
import Shapes

-- | For dealing with non-co/contravariance, see Dolan sec. 9.1
data Range shim t pq where
    MkRange :: (InKind p, InKind q, InKind t) => shim p t -> shim t q -> Range shim t '( p, q)

rangeCo :: Range shim t '( p, q) -> shim t q
rangeCo (MkRange _ tq) = tq

rangeContra :: Range shim t '( p, q) -> shim p t
rangeContra (MkRange pt _) = pt

identityRange :: (InCategory shim, InKind t) => Range shim t '( t, t)
identityRange = MkRange cid cid

pairRange :: Shim shim => Range shim a '( ap, aq) -> Range shim b '( bp, bq) -> Range shim (a, b) '( (ap, bp), (aq, bq))
pairRange (MkRange pa aq) (MkRange pb bq) = MkRange (pairShim pa pb) (pairShim aq bq)

eitherRange ::
       Shim shim
    => Range shim a '( ap, aq)
    -> Range shim b '( bp, bq)
    -> Range shim (Either a b) '( Either ap bp, Either aq bq)
eitherRange (MkRange pa aq) (MkRange pb bq) = MkRange (eitherShim pa pb) (eitherShim aq bq)

coRange :: JoinMeetCategory shim => shim t q -> Range shim t '( BottomType, q)
coRange tq = MkRange initf tq

contraRange :: JoinMeetCategory shim => shim p t -> Range shim t '( p, TopType)
contraRange pt = MkRange pt termf

rangeBijection :: Range shim a '( b, b) -> Isomorphism shim a b
rangeBijection (MkRange ba ab) = MkIsomorphism ab ba

bijectionRange :: (InKind a, InKind b) => Isomorphism shim a b -> Range shim a '( b, b)
bijectionRange (MkIsomorphism ab ba) = MkRange ba ab

bijectRanges :: InCategory shim => Range shim a '( p, q) -> Range shim b '( q, p) -> Isomorphism shim a b
bijectRanges (MkRange pa aq) (MkRange qb bp) = MkIsomorphism (qb <.> aq) (pa <.> bp)

unifyRange1 :: InCategory shim => shim q p -> Range shim t '( p, q) -> Range shim t '( p, p)
unifyRange1 qp (MkRange pt tq) = MkRange pt (qp <.> tq)

unifyRange2 :: InCategory shim => shim q p -> Range shim t '( p, q) -> Range shim t '( q, q)
unifyRange2 qp (MkRange pt tq) = MkRange (pt <.> qp) tq

unUnifyRange1 :: (InCategory shim, InKind q) => shim p q -> Range shim t '( p, p) -> Range shim t '( p, q)
unUnifyRange1 pq (MkRange pt tp) = MkRange pt (pq <.> tp)

unUnifyRange2 :: (InCategory shim, InKind p) => shim p q -> Range shim t '( q, q) -> Range shim t '( p, q)
unUnifyRange2 pq (MkRange qt tq) = MkRange (qt <.> pq) tq

unjoinRange ::
       JoinMeetCategory shim => Range shim t '( JoinType p1 p2, q) -> (Range shim t '( p1, q), Range shim t '( p2, q))
unjoinRange tr = (contraMapWithRange join1 tr, contraMapWithRange join2 tr)

data CatRange (shim :: k -> k -> Type) (pq1 :: (k, k)) (pq2 :: (k, k)) where
    MkCatRange
        :: (InKind p1, InKind p2, InKind q1, InKind q2)
        => shim p2 p1
        -> shim q1 q2
        -> CatRange shim '( p1, q1) '( p2, q2)

instance (forall a b. Show (shim a b)) => Show (CatRange shim a' b') where
    show (MkCatRange p q) = "(" <> show p <> "," <> show q <> ")"

coCatRange :: (InCategory shim, InKind p, InKind q1, InKind q2) => shim q1 q2 -> CatRange shim '( p, q1) '( p, q2)
coCatRange qq = MkCatRange cid qq

contraCatRange :: (InCategory shim, InKind p1, InKind p2, InKind q) => shim p2 p1 -> CatRange shim '( p1, q) '( p2, q)
contraCatRange pp = MkCatRange pp cid

instance InCategory shim => InCategory (CatRange shim) where
    cid :: forall a. InKind a
        => CatRange shim a a
    cid =
        case inKind @_ @a of
            MkPairWitness -> MkCatRange cid cid
    (<.>) ::
           forall a b c. (InKind a, InKind b, InKind c)
        => CatRange shim b c
        -> CatRange shim a b
        -> CatRange shim a c
    MkCatRange pa qa <.> MkCatRange pb qb =
        case (inKind @_ @a, inKind @_ @b, inKind @_ @c) of
            (MkPairWitness, MkPairWitness, MkPairWitness) -> MkCatRange (pb <.> pa) (qa <.> qb)

instance InGroupoid cat => InGroupoid (CatRange cat) where
    cinvert (MkCatRange p q) = MkCatRange (cinvert p) (cinvert q)

mapWithRange :: InCategory shim => CatRange shim a b -> Range shim t a -> Range shim t b
mapWithRange (MkCatRange pp qq) (MkRange pt tq) = MkRange (pt <.> pp) (qq <.> tq)

coMapWithRange ::
       (InCategory shim, InKind p, InKind q1, InKind q2)
    => shim q1 q2
    -> Range shim t '( p, q1)
    -> Range shim t '( p, q2)
coMapWithRange qq = mapWithRange $ coCatRange qq

contraMapWithRange ::
       (InCategory shim, InKind p1, InKind p2, InKind q)
    => shim p2 p1
    -> Range shim t '( p1, q)
    -> Range shim t '( p2, q)
contraMapWithRange pp = mapWithRange $ contraCatRange pp

data RangeType tw polarity (pq :: (Type, Type)) where
    MkRangeType :: tw (InvertPolarity polarity) p -> tw polarity q -> RangeType tw polarity '( p, q)

rangeTypeInKind :: forall tw polarity. Subrepresentative (RangeType tw polarity) (KindWitness (Type, Type))
rangeTypeInKind (MkRangeType _ _) = Dict
