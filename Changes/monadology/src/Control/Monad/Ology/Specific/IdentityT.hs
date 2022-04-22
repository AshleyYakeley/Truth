{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.IdentityT
    ( module Control.Monad.Trans.Identity
    , module Control.Monad.Ology.Specific.IdentityT
    ) where

import Control.Monad.Ology.General
import Control.Monad.Trans.Identity hiding (liftCallCC, liftCatch)
import Import

instance TransConstraint Functor IdentityT where
    hasTransConstraint = Dict

instance TransConstraint Applicative IdentityT where
    hasTransConstraint = Dict

instance TransConstraint Monad IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadIO IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadFail IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadFix IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus IdentityT where
    hasTransConstraint = Dict

instance TransConstraint MonadOuter IdentityT where
    hasTransConstraint = Dict

instance MonadExtract m => MonadExtract (IdentityT m) where
    mToValue (IdentityT ma) = mToValue ma

instance TransConstraint MonadExtract IdentityT where
    hasTransConstraint = Dict

instance MonadException m => MonadException (IdentityT m) where
    type Exc (IdentityT m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc tma handler = tunnel $ \unlift -> catchExc (unlift tma) $ \e -> unlift $ handler e

instance TransConstraint MonadException IdentityT where
    hasTransConstraint = Dict

instance MonadThrow e m => MonadThrow e (IdentityT m) where
    throw e = lift $ throw e

instance TransConstraint (MonadThrow e) IdentityT where
    hasTransConstraint = Dict

instance MonadCatch e m => MonadCatch e (IdentityT m) where
    catch ma handler = tunnel $ \unlift -> catch (unlift ma) $ \e -> unlift $ handler e

instance TransConstraint (MonadCatch e) IdentityT where
    hasTransConstraint = Dict

instance MonadInner m => MonadInner (IdentityT m) where
    retrieveInner (IdentityT ma) = retrieveInner ma

instance TransConstraint MonadInner IdentityT where
    hasTransConstraint = Dict

instance MonadIdentity m => MonadIdentity (IdentityT m)

instance TransConstraint MonadIdentity IdentityT where
    hasTransConstraint = Dict

instance MonadTransCoerce IdentityT where
    transCoerce = Dict

instance MonadTransTunnel IdentityT where
    type Tunnel IdentityT = Identity
    tunnel call = IdentityT $ fmap runIdentity $ call $ \(IdentityT ma) -> fmap Identity ma

instance MonadOuter m => MonadOuter (IdentityT m) where
    getExtract =
        IdentityT $ do
            MkExtract maa <- getExtract
            return $ MkExtract $ maa . runIdentityT

instance MonadTransUnlift IdentityT where
    liftWithUnlift call = IdentityT $ call runIdentityT

instance MonadTransAskUnlift IdentityT

identityWUnliftAll :: WUnlift c IdentityT
identityWUnliftAll = MkWUnlift runIdentityT
