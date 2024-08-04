module Pinafore.Base.KnowShim where

import Pinafore.Base.Know
import Shapes

type KnowShim :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data KnowShim w f t where
    MkKnowShim :: w dt -> f (dt -> Know t) -> KnowShim w f t

instance Functor f => Functor (KnowShim w f) where
    fmap ab (MkKnowShim def f) = MkKnowShim def (fmap (\dtk -> fmap ab . dtk) f)

instance (forall dt. Show (w dt)) => Show (KnowShim w f t) where
    show (MkKnowShim wdt _) = show wdt

simpleKnowShim :: Applicative f => w t -> KnowShim w f t
simpleKnowShim wt = MkKnowShim wt $ pure Known

convertKnowShim :: (forall dt. w1 dt -> w2 dt) -> KnowShim w1 f t -> KnowShim w2 f t
convertKnowShim f (MkKnowShim wt conv) = MkKnowShim (f wt) conv

gateKnowShim :: Applicative f => f (t -> Bool) -> KnowShim w f t -> KnowShim w f t
gateKnowShim fprd (MkKnowShim def ff) = let
    gateKnow :: forall t. (t -> Bool) -> Know t -> Know t
    gateKnow f (Known t)
        | f t = Known t
    gateKnow _ _ = Unknown
    in MkKnowShim def $ liftA2 (\prd f dt -> gateKnow prd (f dt)) fprd ff
