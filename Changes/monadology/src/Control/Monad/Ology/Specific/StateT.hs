{-# OPTIONS -fno-warn-orphans #-}

module Control.Monad.Ology.Specific.StateT
    ( module Control.Monad.Trans.State
    , module Control.Monad.Ology.Specific.StateT
    ) where

import Control.Monad.Ology.General
import Control.Monad.Trans.State hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Import

instance TransConstraint Functor (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint Monad (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadFix (StateT s) where
    hasTransConstraint = Dict

instance TransConstraint MonadPlus (StateT s) where
    hasTransConstraint = Dict

instance MonadTransCoerce (StateT a) where
    transCoerce = Dict

instance MonadException m => MonadException (StateT s m) where
    type Exc (StateT s m) = Exc m
    throwExc e = lift $ throwExc e
    catchExc tma handler = tunnel $ \unlift -> catchExc (unlift tma) $ \e -> unlift $ handler e

instance TransConstraint MonadException (StateT s) where
    hasTransConstraint = Dict

instance MonadThrow e m => MonadThrow e (StateT s m) where
    throw e = lift $ throw e

instance TransConstraint (MonadThrow e) (StateT s) where
    hasTransConstraint = Dict

instance MonadCatch e m => MonadCatch e (StateT s m) where
    catch ma handler = tunnel $ \unlift -> catch (unlift ma) $ \e -> unlift $ handler e

instance TransConstraint (MonadCatch e) (StateT s) where
    hasTransConstraint = Dict

instance MonadTransTunnel (StateT s) where
    type Tunnel (StateT s) = (,) (Endo s)
    tunnel call =
        StateT $ \olds ->
            fmap (\(Endo sf, r) -> (r, sf olds)) $
            call $ \(StateT smrs) -> fmap (\(a, s) -> (Endo $ pure s, a)) $ smrs olds

instance MonadTransUnlift (StateT s) where
    liftWithUnlift call = liftWithMVarStateT $ \var -> call $ mVarRun var

mVarRun :: MVar s -> Unlift MonadTunnelIO (StateT s)
mVarRun var (StateT smr) =
    tunnelIO $ \unlift ->
        modifyMVar var $ \olds ->
            fmap (\fas -> (fromMaybe olds $ mToMaybe $ fmap snd fas, fmap fst fas)) $ unlift $ smr olds

mVarUnitRun :: MonadTunnelIO m => MVar s -> m --> m
mVarUnitRun var ma = mVarRun var $ lift ma

mVarUnitUnlock :: MVar () -> IO --> IO
mVarUnitUnlock var = bracket_ (putMVar var ()) (takeMVar var)

stateDiscardingUntrans :: s -> Unlift MonadIO (StateT s)
stateDiscardingUntrans s mr = do
    (r, _discarded) <- runStateT mr s
    return r

-- | Dangerous, because the MVar won't be released on exception.
dangerousMVarRun :: MVar s -> Unlift MonadIO (StateT s)
dangerousMVarRun var (StateT smr) = do
    olds <- liftIO $ takeMVar var
    (a, news) <- smr olds
    liftIO $ putMVar var news
    return a

liftStateT :: (Traversable f, Applicative m) => StateT s m a -> StateT (f s) m (f a)
liftStateT (StateT smas) = StateT $ \fs -> fmap (\fas -> (fmap fst fas, fmap snd fas)) $ traverse smas fs

liftWithMVarStateT :: MonadIO m => (MVar s -> m a) -> StateT s m a
liftWithMVarStateT vma =
    StateT $ \initialstate -> do
        var <- liftIO $ newMVar initialstate
        r <- vma var
        finalstate <- liftIO $ takeMVar var
        return (r, finalstate)

mVarWIORun :: MVar s -> WMFunction (StateT s IO) IO
mVarWIORun var = MkWMFunction $ mVarRun var
