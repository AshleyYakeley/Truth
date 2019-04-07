module Data.Bijection where

import Control.Category.Dual
import Control.Category.Groupoid
import Data.CatFunctor
import Data.IsoVariant
import Data.KindMorphism
import Shapes.Import

data Isomorphism (cat :: k -> k -> Type) (a :: k) (b :: k) = MkIsomorphism
    { isoForwards :: cat a b
    , isoBackwards :: cat b a
    }
    -- isoForwards . isoBackwards = id
    -- isoBackwards . isoForwards = id

type Bijection = Isomorphism (->)

instance InCategory cat => InCategory (Isomorphism cat) where
    cid = MkIsomorphism cid cid
    (MkIsomorphism p1 q1) <.> (MkIsomorphism p2 q2) = MkIsomorphism (p1 <.> p2) (q2 <.> q1)

instance Category cat => Category (Isomorphism cat) where
    id = MkIsomorphism id id
    (MkIsomorphism p1 q1) . (MkIsomorphism p2 q2) = MkIsomorphism (p1 . p2) (q2 . q1)

instance Category cat => Groupoid (Isomorphism cat) where
    invert (MkIsomorphism ab ba) = MkIsomorphism ba ab

instance CatFunctor catp catq f => CatFunctor (Isomorphism catp) (Isomorphism catq) (f :: Type -> Type) where
    cfmap bi = MkIsomorphism {isoForwards = cfmap (isoForwards bi), isoBackwards = cfmap (isoBackwards bi)}

instance CatFunctor (CatDual catp) catq f =>
             CatFunctor (CatDual (Isomorphism catp)) (Isomorphism catq) (f :: Type -> Type) where
    cfmap (MkCatDual bi) =
        MkIsomorphism {isoForwards = ccontramap (isoForwards bi), isoBackwards = ccontramap (isoBackwards bi)}

biIsoMap :: IsoVariant f => Bijection a b -> f a -> f b
biIsoMap (MkIsomorphism ab ba) = isoMap ab ba

biIsoBi :: IsoVariant f => Bijection a b -> Bijection (f a) (f b)
biIsoBi (MkIsomorphism ab ba) = MkIsomorphism (isoMap ab ba) (isoMap ba ab)

biIsoMap' :: IsoVariant' f => Bijection a b -> f a t -> f b t
biIsoMap' (MkIsomorphism ab ba) = isoMap' ab ba

biIsoBi' :: IsoVariant' f => Bijection a b -> Bijection (f a t) (f b t)
biIsoBi' (MkIsomorphism ab ba) = MkIsomorphism (isoMap' ab ba) (isoMap' ba ab)

biSwap :: Bijection (a, b) (b, a)
biSwap = MkIsomorphism swap swap

packBijection :: IsSequence t => Bijection [Element t] t
packBijection = MkIsomorphism pack unpack

unpackBijection :: IsSequence t => Bijection t [Element t]
unpackBijection = MkIsomorphism unpack pack

class HasKindMorphism (k :: Type) where
    kindMorphismMapCat ::
           forall (cat1 :: Type -> Type -> Type) (cat2 :: Type -> Type -> Type) (a :: k) (b :: k).
           (Category cat1, Category cat2)
        => (forall p q. cat1 p q -> cat2 p q)
        -> KindMorphism cat1 a b
        -> KindMorphism cat2 a b
    mkKindBijection ::
           forall (cat :: Type -> Type -> Type) (a :: k) (b :: k). Category cat
        => KindMorphism cat a b
        -> KindMorphism cat b a
        -> KindIsomorphism cat a b

type KindIsomorphism (cat :: Type -> Type -> Type) = KindMorphism (Isomorphism cat)

type KindBijection = KindMorphism Bijection

instance HasKindMorphism Type where
    kindMorphismMapCat ab = ab
    mkKindBijection = MkIsomorphism

instance HasKindMorphism kq => HasKindMorphism (kp -> kq) where
    kindMorphismMapCat ab (MkNestedMorphism a) = MkNestedMorphism $ kindMorphismMapCat ab a
    mkKindBijection ::
           forall cat (a :: kp -> kq) (b :: kp -> kq). Category cat
        => KindMorphism cat a b
        -> KindMorphism cat b a
        -> KindIsomorphism cat a b
    mkKindBijection (MkNestedMorphism ab) (MkNestedMorphism ba) = MkNestedMorphism $ mkKindBijection @_ @cat ab ba

instance HasKindMorphism Constraint where
    kindMorphismMapCat ab (MkConstraintMorphism a) = MkConstraintMorphism $ ab a
    mkKindBijection (MkConstraintMorphism ab) (MkConstraintMorphism ba) = MkConstraintMorphism $ MkIsomorphism ab ba

instance (HasKindMorphism kp, HasKindMorphism kq) => HasKindMorphism (kp, kq) where
    kindMorphismMapCat ab (MkPairMorphism pa qa) = MkPairMorphism (kindMorphismMapCat ab pa) (kindMorphismMapCat ab qa)
    mkKindBijection ::
           forall cat (a :: (kp, kq)) b. Category cat
        => KindMorphism cat a b
        -> KindMorphism cat b a
        -> KindIsomorphism cat a b
    mkKindBijection (MkPairMorphism papb qaqb) (MkPairMorphism pbpa qbqa) =
        MkPairMorphism (mkKindBijection @_ @cat papb pbpa) (mkKindBijection @_ @cat qaqb qbqa)
