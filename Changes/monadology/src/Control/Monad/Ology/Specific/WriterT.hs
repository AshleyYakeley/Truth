{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.WriterT
    ( module Control.Monad.Trans.Writer
    ) where

import Control.Monad.Ology.General
import Control.Monad.Trans.Writer hiding (liftCallCC, liftCatch)
import Import

instance Monoid w => TransConstraint Functor (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => TransConstraint Applicative (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => TransConstraint Monad (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => TransConstraint MonadIO (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => TransConstraint MonadFail (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => TransConstraint MonadFix (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => TransConstraint MonadPlus (WriterT w) where
    hasTransConstraint = Dict

instance (MonadExtract m, Monoid w) => MonadExtract (WriterT w m) where
    mToValue (WriterT maw) = fst $ mToValue maw

instance Monoid w => TransConstraint MonadExtract (WriterT w) where
    hasTransConstraint = Dict

instance (MonadInner m, Monoid w) => MonadInner (WriterT w m) where
    retrieveInner (WriterT maw) = fmap fst $ retrieveInner maw

instance Monoid w => TransConstraint MonadInner (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => MonadTransCoerce (WriterT w) where
    transCoerce = Dict

instance (Monoid w, MonadException m) => MonadException (WriterT w m) where
    type Exc (WriterT w m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc tma handler = tunnel $ \unlift -> catchExc (unlift tma) $ \e -> unlift $ handler e

instance Monoid w => TransConstraint MonadException (WriterT w) where
    hasTransConstraint = Dict

instance (MonadThrow e m, Monoid w) => MonadThrow e (WriterT w m) where
    throw e = lift $ throw e

instance Monoid w => TransConstraint (MonadThrow e) (WriterT w) where
    hasTransConstraint = Dict

instance (MonadCatch e m, Monoid w) => MonadCatch e (WriterT w m) where
    catch ma handler = tunnel $ \unlift -> catch (unlift ma) $ \e -> unlift $ handler e

instance Monoid w => TransConstraint (MonadCatch e) (WriterT w) where
    hasTransConstraint = Dict

instance Monoid w => MonadTransHoist (WriterT w) where
    hoist = tunnelHoist

instance Monoid w => MonadTransTunnel (WriterT w) where
    type Tunnel (WriterT w) = (,) w
    tunnel call = WriterT $ fmap swap $ call $ \(WriterT mrs) -> fmap swap $ mrs

instance Monoid w => MonadTransUnlift (WriterT w) where
    liftWithUnlift call = do
        var <- liftIO $ newMVar mempty
        r <-
            lift $
            call $ \(WriterT mrs) -> do
                (r, output) <- mrs
                liftIO $ modifyMVar_ var $ \oldoutput -> return $ mappend oldoutput output
                return r
        totaloutput <- liftIO $ takeMVar var
        tell totaloutput
        return r
