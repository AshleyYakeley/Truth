module Pinafore.Base.KnowShim where

import Shapes

import Pinafore.Base.Know

type KnowShim :: (Type -> Type) -> Type -> Type
data KnowShim w t where
    MkKnowShim :: w dt -> (dt -> Know t) -> KnowShim w t

instance Functor (KnowShim w) where
    fmap ab (MkKnowShim def f) = MkKnowShim def (fmap ab . f)

instance (forall dt. Show (w dt)) => Show (KnowShim w t) where
    show (MkKnowShim wdt _) = show wdt

simpleKnowShim :: w t -> KnowShim w t
simpleKnowShim wt = MkKnowShim wt Known

convertKnowShim :: (forall dt. w1 dt -> w2 dt) -> KnowShim w1 t -> KnowShim w2 t
convertKnowShim f (MkKnowShim wt conv) = MkKnowShim (f wt) conv

gateKnowShim :: (t -> Bool) -> KnowShim w t -> KnowShim w t
gateKnowShim prd (MkKnowShim def f) = MkKnowShim def $ \dt -> gateKnow prd (f dt)
