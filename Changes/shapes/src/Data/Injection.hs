module Data.Injection where

import Data.CatFunctor
import Data.Codec
import Data.Isomorphism
import Data.MonadOne
import Shapes.Import

data Injection' m a b = MkInjection
    { injForwards :: a -> b
    , injBackwards :: b -> m a
    }

instance IsBiMap Injection' where
    mapBiMapM ff inj = MkInjection {injForwards = injForwards inj, injBackwards = \b -> ff (injBackwards inj b)}

type Injection = Injection' Maybe

instance Category Injection where
    id = MkInjection {injForwards = id, injBackwards = Just}
    bc . ab =
        MkInjection
            { injForwards = (injForwards bc) . (injForwards ab)
            , injBackwards =
                  \c -> do
                      b <- injBackwards bc c
                      injBackwards ab b
            }

instance (Traversable f) => CatFunctor Injection Injection f where
    cfmap lens = MkInjection {injForwards = fmap (injForwards lens), injBackwards = traverse (injBackwards lens)}

hoistInjection :: (forall t. m1 t -> m2 t) -> Injection' m1 a b -> Injection' m2 a b
hoistInjection ff (MkInjection ab bma) = MkInjection ab $ ff . bma

toInjection :: MonadOne m => Injection' m a b -> Injection a b
toInjection = hoistInjection getMaybeOne

codecInjection :: (Functor m) => Codec' m a b -> Injection' m a (m b)
codecInjection codec = MkInjection {injForwards = decode codec, injBackwards = fmap (encode codec)}

bijectionInjection :: Bijection a b -> Injection' Identity a b
bijectionInjection bi = MkInjection {injForwards = isoForwards bi, injBackwards = Identity . (isoBackwards bi)}
