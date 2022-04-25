{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.ExceptT
    ( module Control.Monad.Trans.Except
    , module Control.Monad.Ology.Specific.ExceptT
    ) where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.Result
import Control.Monad.Trans.Except hiding (liftCallCC, liftListen, liftPass)
import Import

instance TransConstraint Functor (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint Monad (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (ExceptT e) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (ExceptT e) where
    hasTransConstraint = Dict

instance Monoid e => TransConstraint MonadPlus (ExceptT e) where
    hasTransConstraint = Dict

instance MonadInner m => MonadInner (ExceptT e m) where
    retrieveInner (ExceptT meea) =
        case retrieveInner meea of
            SuccessResult (Right a) -> SuccessResult a
            SuccessResult (Left e) -> FailureResult $ Left e
            FailureResult e -> FailureResult $ Right e

instance TransConstraint MonadInner (ExceptT e) where
    hasTransConstraint = Dict

instance MonadTransCoerce (ExceptT e) where
    transCoerce = Dict

instance MonadException m => MonadException (ExceptT e m) where
    type Exc (ExceptT e m) = Either e (Exc m)
    throwExc (Left e) = ExceptT $ return $ Left e
    throwExc (Right e) = ExceptT $ throwExc e
    catchExc (ExceptT mea) handler =
        ExceptT $ do
            ea <- tryExc mea
            case ea of
                FailureResult e -> runExceptT $ handler $ Right e
                SuccessResult (Left e) -> runExceptT $ handler $ Left e
                SuccessResult (Right a) -> return $ return a

instance TransConstraint MonadException (ExceptT e) where
    hasTransConstraint = Dict

instance MonadThrow ex m => MonadThrow (Either e ex) (ExceptT e m) where
    throw (Left e) = ExceptT $ return $ Left e
    throw (Right e) = ExceptT $ throw e

instance MonadCatch ex m => MonadCatch (Either e ex) (ExceptT e m) where
    catch (ExceptT mea) handler =
        ExceptT $ do
            ea <- try mea
            case ea of
                FailureResult e -> runExceptT $ handler $ Right e
                SuccessResult (Left e) -> runExceptT $ handler $ Left e
                SuccessResult (Right a) -> return $ return a

transExcept ::
       forall t m e a. (MonadTransTunnel t, Applicative (Tunnel t), Monad m)
    => t (ExceptT e m) a
    -> t m (Either e a)
transExcept tema = tunnel $ \unlift -> fmap commuteInner $ runExceptT $ unlift tema

instance MonadTransTunnel (ExceptT e) where
    type Tunnel (ExceptT e) = Either e
    tunnel call = ExceptT $ call $ \(ExceptT ma) -> ma
