{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.MaybeT
    ( module Control.Monad.Trans.Maybe
    ) where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.Result
import Control.Monad.Trans.Maybe hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Import

instance TransConstraint Functor MaybeT where
    hasTransConstraint = Dict

instance TransConstraint Monad MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadIO MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadFail MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadFix MaybeT where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus MaybeT where
    hasTransConstraint = Dict

instance MonadException m => MonadException (MaybeT m) where
    type Exc (MaybeT m) = Maybe (Exc m)
    throwExc Nothing = MaybeT $ return Nothing
    throwExc (Just e) = MaybeT $ throwExc e
    catchExc (MaybeT mea) handler =
        MaybeT $ do
            ea <- tryExc mea
            case ea of
                FailureResult e -> runMaybeT $ handler $ Just e
                SuccessResult Nothing -> runMaybeT $ handler Nothing
                SuccessResult (Just a) -> return $ return a

instance MonadThrow e m => MonadThrow (Maybe e) (MaybeT m) where
    throw Nothing = MaybeT $ return Nothing
    throw (Just e) = MaybeT $ throw e

instance MonadCatch e m => MonadCatch (Maybe e) (MaybeT m) where
    catch (MaybeT mea) handler =
        MaybeT $ do
            ea <- try mea
            case ea of
                FailureResult e -> runMaybeT $ handler $ Just e
                SuccessResult Nothing -> runMaybeT $ handler Nothing
                SuccessResult (Just a) -> return $ return a

instance MonadInner m => MonadInner (MaybeT m) where
    retrieveInner (MaybeT mma) =
        case retrieveInner mma of
            SuccessResult (Just a) -> SuccessResult a
            SuccessResult Nothing -> FailureResult Nothing
            FailureResult e -> FailureResult $ Just e

instance TransConstraint MonadInner MaybeT where
    hasTransConstraint = Dict

instance MonadTransCoerce MaybeT where
    transCoerce = Dict

instance MonadTransTunnel MaybeT where
    type Tunnel MaybeT = Maybe
    tunnel call = MaybeT $ call $ \(MaybeT ma) -> ma
