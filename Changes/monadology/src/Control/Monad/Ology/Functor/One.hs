module Control.Monad.Ology.Functor.One where

import Import

class Functor f => FunctorOne f where
    -- | When used on Tunnels, this discards effects.
    fpure :: forall a. a -> f a
    default fpure :: Applicative f => forall a. a -> f a
    fpure = pure
    -- | must satisfy @fextractm . fpure = Just@
    fextractm :: forall a. f a -> Maybe a

sequenceEither :: FunctorOne f => Either e (f a) -> f (Either e a)
sequenceEither (Left e) = fpure $ Left e
sequenceEither (Right fa) = fmap Right fa

instance FunctorOne Identity where
    fextractm (Identity a) = Just a

instance FunctorOne Maybe where
    fextractm = id

instance Monoid p => FunctorOne ((,) p) where
    fextractm (_, a) = Just a

instance FunctorOne (Either p) where
    fextractm (Left _) = Nothing
    fextractm (Right a) = Just a

instance (FunctorOne f1, FunctorOne f2) => FunctorOne (Compose f1 f2) where
    fpure a = Compose $ fpure $ fpure a
    fextractm (Compose ffa) = fextractm ffa >>= fextractm

class FunctorOne f => FunctorExtract f where
    -- | must satisfy @fextract . fpure = id@, @fextractm = Just . fextract@
    fextract :: forall a. f a -> a

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

-- | When used on Tunnels, this discards effects in tunnel @fb@ (due to use of @fpure@)
fcommuteADiscard :: (Functor fa, FunctorExtract fb) => fa (fb r) -> fb (fa r)
fcommuteADiscard abr = fpure $ fmap fextract abr

fcommuteB :: (FunctorExtract fa, Functor fb) => fa (fb r) -> fb (fa r)
fcommuteB abr = fmap (\r -> fmap (\_ -> r) abr) $ fextract abr
