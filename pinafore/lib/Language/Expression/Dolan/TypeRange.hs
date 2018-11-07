module Language.Expression.Dolan.TypeRange where

import Language.Expression.Dolan.Polarity
import Shapes

-- | For dealing with non-co/contravariance, see Dolan sec. 9.1
data TypeRange t pq where
    MkTypeRange :: (p -> t) -> (t -> q) -> TypeRange t '( p, q)

typeRangeCo :: TypeRange t '( p, q) -> t -> q
typeRangeCo (MkTypeRange _ tq) = tq

typeRangeContra :: TypeRange t '( p, q) -> p -> t
typeRangeContra (MkTypeRange pt _) = pt

identityTypeRange :: TypeRange t '( t, t)
identityTypeRange = MkTypeRange id id

coTypeRange :: (t -> q) -> TypeRange t '( BottomType, q)
coTypeRange tq = MkTypeRange never tq

contraTypeRange :: (p -> t) -> TypeRange t '( p, TopType)
contraTypeRange pt = MkTypeRange pt alwaysTop

typeRangeBijection :: TypeRange a '( b, b) -> Bijection a b
typeRangeBijection (MkTypeRange ba ab) = MkBijection ab ba

bijectionTypeRange :: Bijection a b -> TypeRange a '( b, b)
bijectionTypeRange (MkBijection ab ba) = MkTypeRange ba ab

bijectTypeRanges :: TypeRange a '( p, q) -> TypeRange b '( q, p) -> Bijection a b
bijectTypeRanges (MkTypeRange pa aq) (MkTypeRange qb bp) = MkBijection (qb . aq) (pa . bp)

unifyTypeRange1 :: (q -> p) -> TypeRange t '( p, q) -> TypeRange t '( p, p)
unifyTypeRange1 qp (MkTypeRange pt tq) = MkTypeRange pt (qp . tq)

unifyTypeRange2 :: (q -> p) -> TypeRange t '( p, q) -> TypeRange t '( q, q)
unifyTypeRange2 qp (MkTypeRange pt tq) = MkTypeRange (pt . qp) tq

unUnifyTypeRange1 :: (p -> q) -> TypeRange t '( p, p) -> TypeRange t '( p, q)
unUnifyTypeRange1 pq (MkTypeRange pt tp) = MkTypeRange pt (pq . tp)

unUnifyTypeRange2 :: (p -> q) -> TypeRange t '( q, q) -> TypeRange t '( p, q)
unUnifyTypeRange2 pq (MkTypeRange qt tq) = MkTypeRange (qt . pq) tq

unjoinTypeRange :: TypeRange t '( JoinType p1 p2, q) -> (TypeRange t '( p1, q), TypeRange t '( p2, q))
unjoinTypeRange tr = (contraMapTypeRange join1 tr, contraMapTypeRange join2 tr)

data WithRange cat pq1 pq2 where
    MkWithRange :: cat p2 p1 -> cat q1 q2 -> WithRange cat '( p1, q1) '( p2, q2)

coWithRange :: Category cat => cat q1 q2 -> WithRange cat '( p, q1) '( p, q2)
coWithRange qq = MkWithRange id qq

contraWithRange :: Category cat => cat p2 p1 -> WithRange cat '( p1, q) '( p2, q)
contraWithRange pp = MkWithRange pp id

-- Sadly we can't define 'instance Category (WithRange cat)'
invertWithRange :: Groupoid cat => WithRange cat a b -> WithRange cat b a
invertWithRange (MkWithRange p q) = MkWithRange (invert p) (invert q)

class IsoMapTypeRange f where
    isoMapTypeRange :: WithRange Bijection a b -> f a -> f b
    default isoMapTypeRange :: MapTypeRange f => WithRange Bijection a b -> f a -> f b
    isoMapTypeRange (MkWithRange a b) = mapTypeRange $ MkWithRange (biForwards a) (biForwards b)

isoBiTypeRange :: IsoMapTypeRange f => WithRange Bijection a b -> Bijection (f a) (f b)
isoBiTypeRange rbij = MkBijection (isoMapTypeRange rbij) (isoMapTypeRange $ invertWithRange rbij)

class IsoMapTypeRange f => MapTypeRange f where
    mapTypeRange :: WithRange (->) a b -> f a -> f b

coMapTypeRange :: MapTypeRange f => (q1 -> q2) -> f '( p, q1) -> f '( p, q2)
coMapTypeRange qq = mapTypeRange $ coWithRange qq

contraMapTypeRange :: MapTypeRange f => (p2 -> p1) -> f '( p1, q) -> f '( p2, q)
contraMapTypeRange pp = mapTypeRange $ contraWithRange pp

instance IsoMapTypeRange (TypeRange t)

instance MapTypeRange (TypeRange t) where
    mapTypeRange (MkWithRange pp qq) (MkTypeRange pt tq) = MkTypeRange (pt . pp) (qq . tq)

class IsoMapTypeRange' f where
    isoMapTypeRange' :: WithRange Bijection a b -> f a t -> f b t
    default isoMapTypeRange' :: MapTypeRange' f => WithRange Bijection a b -> f a t -> f b t
    isoMapTypeRange' (MkWithRange a b) = mapTypeRange' $ MkWithRange (biForwards a) (biForwards b)

isoBiTypeRange' :: IsoMapTypeRange' f => WithRange Bijection a b -> Bijection (f a t) (f b t)
isoBiTypeRange' rbij = MkBijection (isoMapTypeRange' rbij) (isoMapTypeRange' $ invertWithRange rbij)

class IsoMapTypeRange' f => MapTypeRange' f where
    mapTypeRange' :: WithRange (->) a b -> f a t -> f b t

coMapTypeRange' :: MapTypeRange' f => (q1 -> q2) -> f '( p, q1) t -> f '( p, q2) t
coMapTypeRange' qq = mapTypeRange' $ coWithRange qq

contraMapTypeRange' :: MapTypeRange' f => (p2 -> p1) -> f '( p1, q) t -> f '( p2, q) t
contraMapTypeRange' pp = mapTypeRange' $ contraWithRange pp

data TypeRangeWitness tw polarity (pq :: (Type, Type)) where
    MkTypeRangeWitness :: tw (InvertPolarity polarity) p -> tw polarity q -> TypeRangeWitness tw polarity '( p, q)

rangeTypeInKind :: forall tw polarity. Subrepresentative (TypeRangeWitness tw polarity) (KindWitness (Type, Type))
rangeTypeInKind (MkTypeRangeWitness _ _) = Dict
