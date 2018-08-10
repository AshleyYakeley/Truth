module Language.Expression.Dolan.TypeRange where

import Language.Expression.Dolan.Polarity
import Shapes

-- | For dealing with non-co/contravariance, see Dolan sec. 9.1
data TypeRange t pq where
    MkTypeRange :: (p -> t) -> (t -> q) -> TypeRange t '( p, q)

data WithRange cat pq1 pq2 where
    MkWithRange :: cat p2 p1 -> cat q1 q2 -> WithRange cat '( p1, q1) '( p2, q2)

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

instance IsoMapTypeRange (TypeRange t)

instance MapTypeRange (TypeRange t) where
    mapTypeRange (MkWithRange pp qq) (MkTypeRange pt tq) = MkTypeRange (pt . pp) (qq . tq)

class IsoMapTypeRange' f where
    isoMapTypeRange' :: WithRange Bijection a b -> f a t -> f b t
    default isoMapTypeRange' :: MapTypeRange' f => WithRange Bijection a b -> f a t -> f b t
    isoMapTypeRange' (MkWithRange a b) = mapTypeRange' $ MkWithRange (biForwards a) (biForwards b)

class IsoMapTypeRange' f => MapTypeRange' f where
    mapTypeRange' :: WithRange (->) a b -> f a t -> f b t

isoBiTypeRange' :: IsoMapTypeRange' f => WithRange Bijection a b -> Bijection (f a t) (f b t)
isoBiTypeRange' rbij = MkBijection (isoMapTypeRange' rbij) (isoMapTypeRange' $ invertWithRange rbij)

data TypeRangeWitness tw polarity pq where
    MkTypeRangeWitness :: tw (InvertPolarity polarity) p -> tw polarity q -> TypeRangeWitness tw polarity '( p, q)
