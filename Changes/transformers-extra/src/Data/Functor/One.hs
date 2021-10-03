module Data.Functor.One where

import Import

class Functor f => FunctorOne f where
    getMaybeOne :: forall a. f a -> Maybe a

instance FunctorOne Identity where
    getMaybeOne (Identity a) = Just a

instance FunctorOne Maybe where
    getMaybeOne = id

instance FunctorOne ((,) p) where
    getMaybeOne (_, a) = Just a

instance FunctorOne (Either p) where
    getMaybeOne (Left _) = Nothing
    getMaybeOne (Right a) = Just a

instance (FunctorOne f1, FunctorOne f2) => FunctorOne (Compose f1 f2) where
    getMaybeOne (Compose ffa) = getMaybeOne ffa >>= getMaybeOne
