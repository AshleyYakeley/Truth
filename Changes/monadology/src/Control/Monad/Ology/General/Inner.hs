{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.General.Inner where

import Control.Monad.Ology.General.Exception.Class
import Control.Monad.Ology.Specific.Result
import Import

-- | Instances of this type are isomorphic to @Either P (Q,a)@ for some types @P@ and @Q@ (with @Monoid Q@).
-- Must satisfy:
-- * @retrieveInner (fmap f w) = fmap f (retrieveInner w)@
-- * @case (retrieveInner w) of {Left w' -> fmap absurd w'; Right a -> fmap (\_ -> a) w;} = w@
class (Traversable m, MonadException m) => MonadInner m where
    retrieveInner :: forall a. m a -> Result (Exc m) a

instance MonadInner Identity where
    retrieveInner (Identity a) = SuccessResult a

instance MonadInner Maybe where
    retrieveInner (Just a) = SuccessResult a
    retrieveInner Nothing = FailureResult ()

instance MonadInner (Either p) where
    retrieveInner (Right b) = SuccessResult b
    retrieveInner (Left a) = FailureResult a

instance Monoid p => MonadInner ((,) p) where
    retrieveInner (_, a) = SuccessResult a

instance MonadInner (Result e) where
    retrieveInner ra = ra

mToMaybe ::
       forall m a. MonadInner m
    => m a
    -> Maybe a
mToMaybe = resultToMaybe . retrieveInner

commuteInner ::
       forall m f a. (MonadInner m, Applicative f)
    => m (f a)
    -> f (m a)
commuteInner mfa =
    case retrieveInner mfa of
        SuccessResult fa -> fmap pure fa
        FailureResult ex -> pure $ throwExc ex
