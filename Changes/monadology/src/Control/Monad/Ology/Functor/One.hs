module Control.Monad.Ology.Functor.One where

import Import

class Functor f => FunctorOne f where
    -- | When used on Tunnels, this discards effects.
    fpure :: forall a. a -> f a
    default fpure :: Applicative f => forall a. a -> f a
    fpure = pure
    -- | must satisfy @getMaybeOne . fpure = Just@
    getMaybeOne :: forall a. f a -> Maybe a

sequenceEither :: FunctorOne f => Either e (f a) -> f (Either e a)
sequenceEither (Left e) = fpure $ Left e
sequenceEither (Right fa) = fmap Right fa

instance FunctorOne Identity where
    getMaybeOne (Identity a) = Just a

instance FunctorOne Maybe where
    getMaybeOne = id

instance Monoid p => FunctorOne ((,) p) where
    getMaybeOne (_, a) = Just a

instance FunctorOne (Either p) where
    getMaybeOne (Left _) = Nothing
    getMaybeOne (Right a) = Just a

instance (FunctorOne f1, FunctorOne f2) => FunctorOne (Compose f1 f2) where
    fpure a = Compose $ fpure $ fpure a
    getMaybeOne (Compose ffa) = getMaybeOne ffa >>= getMaybeOne

class FunctorOne f => FunctorExtract f where
    -- | must satisfy @fextract . fpure = id@, @getMaybeOne = Just . fextract@
    fextract :: forall a. f a -> a

fcommuteB :: (FunctorExtract fa, Functor fb) => fa (fb r) -> fb (fa r)
fcommuteB abr = fmap (\r -> fmap (\_ -> r) abr) $ fextract abr

fcommuteA :: (Functor fa, FunctorIdentity fb) => fa (fb r) -> fb (fa r)
fcommuteA abr = fpure $ fmap fextract abr

instance FunctorExtract Identity where
    fextract = runIdentity

instance Monoid p => FunctorExtract ((,) p) where
    fextract = snd

instance (FunctorExtract f1, FunctorExtract f2) => FunctorExtract (Compose f1 f2) where
    fextract (Compose ffa) = fextract $ fextract ffa

-- | must satisfy @fpure . fextract = id@, and so be equivalent to the identity functor
class FunctorExtract f => FunctorIdentity f

instance FunctorIdentity Identity

instance (FunctorIdentity f1, FunctorIdentity f2) => FunctorIdentity (Compose f1 f2)
