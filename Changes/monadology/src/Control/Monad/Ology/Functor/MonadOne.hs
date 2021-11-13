{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Functor.MonadOne where

import Control.Monad.Ology.Functor.One
import Control.Monad.Ology.Result
import Import

class (Traversable f, Monad f, FunctorPure f, FunctorOne f) => MonadOne f where
    retrieveOne :: f a -> Result (f None) a
    -- retrieveOne (fmap f w) = fmap f (retrieveOne w)
    -- case (retrieveOne w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w

restoreOne :: MonadOne f => Result (f None) a -> f a
restoreOne (SuccessResult a) = pure a
restoreOne (FailureResult fn) = fmap never fn

traverseOne :: (MonadOne f, Applicative m) => (a -> m b) -> f a -> m (f b)
traverseOne amb fa =
    case retrieveOne fa of
        SuccessResult a -> fmap (\b -> fmap (\_ -> b) fa) (amb a)
        FailureResult fn -> pure $ fmap never fn

sequenceAOne :: (MonadOne f, Applicative m) => f (m a) -> m (f a)
sequenceAOne fma =
    case retrieveOne fma of
        SuccessResult ma -> fmap (\b -> fmap (\_ -> b) fma) ma
        FailureResult fn -> pure $ fmap never fn

bindOne :: (MonadOne f) => f a -> (a -> f b) -> f b
bindOne fa afb =
    case retrieveOne fa of
        SuccessResult a -> afb a
        FailureResult fn -> fmap never fn

fromOne :: MonadOne f => a -> f a -> a
fromOne def fa = fromMaybe def $ getMaybeOne fa

fcommuteOne :: (MonadOne fa, FunctorPure fb) => fa (fb r) -> fb (fa r)
fcommuteOne abr =
    case retrieveOne abr of
        SuccessResult br -> fmap pure br
        FailureResult fv -> fpure $ fmap never fv

instance MonadOne Identity where
    retrieveOne (Identity a) = SuccessResult a

instance MonadOne Maybe where
    retrieveOne (Just a) = SuccessResult a
    retrieveOne Nothing = FailureResult Nothing

instance MonadOne (Either p) where
    retrieveOne (Right b) = SuccessResult b
    retrieveOne (Left a) = FailureResult $ Left a

instance Monoid p => MonadOne ((,) p) where
    retrieveOne (_, a) = SuccessResult a

instance MonadOne (Result e) where
    retrieveOne (SuccessResult a) = SuccessResult a
    retrieveOne (FailureResult e) = FailureResult (FailureResult e)
