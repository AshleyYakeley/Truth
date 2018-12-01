module Language.Expression.Dolan.Range where

import Language.Expression.Dolan.Polarity
import Shapes

-- | For dealing with non-co/contravariance, see Dolan sec. 9.1
data Range t pq where
    MkRange :: (p -> t) -> (t -> q) -> Range t '( p, q)

rangeCo :: Range t '( p, q) -> t -> q
rangeCo (MkRange _ tq) = tq

rangeContra :: Range t '( p, q) -> p -> t
rangeContra (MkRange pt _) = pt

identityRange :: Range t '( t, t)
identityRange = MkRange id id

pairRange :: Range a '( ap, aq) -> Range b '( bp, bq) -> Range (a, b) '( (ap, bp), (aq, bq))
pairRange (MkRange pa aq) (MkRange pb bq) = MkRange (\(a, b) -> (pa a, pb b)) (\(a, b) -> (aq a, bq b))

eitherRange :: Range a '( ap, aq) -> Range b '( bp, bq) -> Range (Either a b) '( Either ap bp, Either aq bq)
eitherRange (MkRange pa aq) (MkRange pb bq) =
    MkRange (either (Left . pa) (Right . pb)) (either (Left . aq) (Right . bq))

coRange :: (t -> q) -> Range t '( BottomType, q)
coRange tq = MkRange never tq

contraRange :: (p -> t) -> Range t '( p, TopType)
contraRange pt = MkRange pt alwaysTop

rangeBijection :: Range a '( b, b) -> Bijection a b
rangeBijection (MkRange ba ab) = MkBijection ab ba

bijectionRange :: Bijection a b -> Range a '( b, b)
bijectionRange (MkBijection ab ba) = MkRange ba ab

bijectRanges :: Range a '( p, q) -> Range b '( q, p) -> Bijection a b
bijectRanges (MkRange pa aq) (MkRange qb bp) = MkBijection (qb . aq) (pa . bp)

unifyRange1 :: (q -> p) -> Range t '( p, q) -> Range t '( p, p)
unifyRange1 qp (MkRange pt tq) = MkRange pt (qp . tq)

unifyRange2 :: (q -> p) -> Range t '( p, q) -> Range t '( q, q)
unifyRange2 qp (MkRange pt tq) = MkRange (pt . qp) tq

unUnifyRange1 :: (p -> q) -> Range t '( p, p) -> Range t '( p, q)
unUnifyRange1 pq (MkRange pt tp) = MkRange pt (pq . tp)

unUnifyRange2 :: (p -> q) -> Range t '( q, q) -> Range t '( p, q)
unUnifyRange2 pq (MkRange qt tq) = MkRange (qt . pq) tq

unjoinRange :: Range t '( JoinType p1 p2, q) -> (Range t '( p1, q), Range t '( p2, q))
unjoinRange tr = (contraMapRange join1 tr, contraMapRange join2 tr)

data WithRange cat pq1 pq2 where
    MkWithRange :: cat p2 p1 -> cat q1 q2 -> WithRange cat '( p1, q1) '( p2, q2)

coWithRange :: Category cat => cat q1 q2 -> WithRange cat '( p, q1) '( p, q2)
coWithRange qq = MkWithRange id qq

contraWithRange :: Category cat => cat p2 p1 -> WithRange cat '( p1, q) '( p2, q)
contraWithRange pp = MkWithRange pp id

-- Sadly we can't define 'instance Category (WithRange cat)'
invertWithRange :: Groupoid cat => WithRange cat a b -> WithRange cat b a
invertWithRange (MkWithRange p q) = MkWithRange (invert p) (invert q)

class IsoMapRange f where
    isoMapRange :: WithRange Bijection a b -> f a -> f b
    default isoMapRange :: MapRange f => WithRange Bijection a b -> f a -> f b
    isoMapRange (MkWithRange a b) = mapRange $ MkWithRange (biForwards a) (biForwards b)

isoBiRange :: IsoMapRange f => WithRange Bijection a b -> Bijection (f a) (f b)
isoBiRange rbij = MkBijection (isoMapRange rbij) (isoMapRange $ invertWithRange rbij)

class IsoMapRange f => MapRange f where
    mapRange :: WithRange (->) a b -> f a -> f b

coMapRange :: MapRange f => (q1 -> q2) -> f '( p, q1) -> f '( p, q2)
coMapRange qq = mapRange $ coWithRange qq

contraMapRange :: MapRange f => (p2 -> p1) -> f '( p1, q) -> f '( p2, q)
contraMapRange pp = mapRange $ contraWithRange pp

instance IsoMapRange (Range t)

instance MapRange (Range t) where
    mapRange (MkWithRange pp qq) (MkRange pt tq) = MkRange (pt . pp) (qq . tq)

class IsoMapRange' f where
    isoMapRange' :: WithRange Bijection a b -> f a t -> f b t
    default isoMapRange' :: MapRange' f => WithRange Bijection a b -> f a t -> f b t
    isoMapRange' (MkWithRange a b) = mapRange' $ MkWithRange (biForwards a) (biForwards b)

isoBiRange' :: IsoMapRange' f => WithRange Bijection a b -> Bijection (f a t) (f b t)
isoBiRange' rbij = MkBijection (isoMapRange' rbij) (isoMapRange' $ invertWithRange rbij)

class IsoMapRange' f => MapRange' f where
    mapRange' :: WithRange (->) a b -> f a t -> f b t

coMapRange' :: MapRange' f => (q1 -> q2) -> f '( p, q1) t -> f '( p, q2) t
coMapRange' qq = mapRange' $ coWithRange qq

contraMapRange' :: MapRange' f => (p2 -> p1) -> f '( p1, q) t -> f '( p2, q) t
contraMapRange' pp = mapRange' $ contraWithRange pp

data RangeType tw polarity (pq :: (Type, Type)) where
    MkRangeType :: tw (InvertPolarity polarity) p -> tw polarity q -> RangeType tw polarity '( p, q)

rangeTypeInKind :: forall tw polarity. Subrepresentative (RangeType tw polarity) (KindWitness (Type, Type))
rangeTypeInKind (MkRangeType _ _) = Dict
