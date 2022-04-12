{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Functor.MonadOne where

import Control.Monad.Ology.Functor.One
import Control.Monad.Ology.Result
import Import

-- | Instances of this type are isomorphic to @Either P (Q,a)@ for some types @P@ and @Q@ (with @Monoid Q@).
class (Traversable f, Monad f, FunctorOne f) => MonadOne f where
    retrieveOne :: f a -> Result (f Void) a
    -- retrieveOne (fmap f w) = fmap f (retrieveOne w)
    -- case (retrieveOne w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w

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

instance (MonadOne m, Monoid w) => MonadOne (WriterT w m) where
    retrieveOne (WriterT maw) =
        case retrieveOne maw of
            SuccessResult (a, _) -> SuccessResult a
            FailureResult mv -> FailureResult $ WriterT $ fmap absurd mv

instance MonadOne m => MonadOne (ExceptT e m) where
    retrieveOne (ExceptT meea) =
        case retrieveOne meea of
            SuccessResult (Right a) -> SuccessResult a
            SuccessResult (Left e) -> FailureResult $ ExceptT $ fmap (\_ -> Left e) meea
            FailureResult mv -> FailureResult $ ExceptT $ fmap Right mv

instance MonadOne m => MonadOne (MaybeT m) where
    retrieveOne (MaybeT mma) =
        case retrieveOne mma of
            SuccessResult (Just a) -> SuccessResult a
            SuccessResult Nothing -> FailureResult $ MaybeT $ fmap (\_ -> Nothing) mma
            FailureResult mv -> FailureResult $ MaybeT $ fmap Just mv
