module Data.SemiIsomorphism where

import Data.Isomorphism
import Data.KindMorphism
import Shapes.Import

data SemiIsomorphism (cat :: k -> k -> Type) (a :: k) (b :: k) = MkSemiIsomorphism
    { semiIsoForwards :: cat a b
    , semiIsoBackwards :: Maybe (cat b a)
    }

instance InCategory cat => InCategory (SemiIsomorphism cat) where
    cid = MkSemiIsomorphism cid $ pure cid
    (MkSemiIsomorphism p1 mq1) <.> (MkSemiIsomorphism p2 mq2) = MkSemiIsomorphism (p1 <.> p2) $ liftA2 (<.>) mq2 mq1

instance Category cat => Category (SemiIsomorphism cat) where
    id = MkSemiIsomorphism id $ pure id
    (MkSemiIsomorphism p1 mq1) . (MkSemiIsomorphism p2 mq2) = MkSemiIsomorphism (p1 . p2) $ liftA2 (.) mq2 mq1

semiIso :: forall k (cat :: k -> k -> Type) (a :: k) (b :: k). cat a b -> SemiIsomorphism cat a b
semiIso semiIsoForwards = let
    semiIsoBackwards = Nothing
    in MkSemiIsomorphism {..}

isoSemiIso :: forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Isomorphism cat a b -> SemiIsomorphism cat a b
isoSemiIso MkIsomorphism {..} = let
    semiIsoForwards = isoForwards
    semiIsoBackwards = Just isoBackwards
    in MkSemiIsomorphism {..}

semiIsoMapCat ::
       forall k (cat1 :: k -> k -> Type) (cat2 :: k -> k -> Type) (p :: k) (q :: k).
       (forall (a :: k) (b :: k). cat1 a b -> cat2 a b)
    -> SemiIsomorphism cat1 p q
    -> SemiIsomorphism cat2 p q
semiIsoMapCat m (MkSemiIsomorphism f b) = MkSemiIsomorphism (m f) (fmap m b)
