module Control.Monad.Trans.Unlift where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Writer
import Data.Constraint
import Data.Kind
import Data.Tuple
import Prelude

newtype Unlift (t :: (* -> *) -> * -> *) = MkUnlift
    { runUnlift :: forall (m :: * -> *) (r :: *). MonadUnliftIO m =>
                                                      t m r -> m r
    }

identityUnlift :: Unlift IdentityT
identityUnlift = MkUnlift runIdentityT

mvarRun :: MonadUnliftIO m => MVar s -> StateT s m r -> m r
mvarRun var (StateT smr) =
    liftIOWithUnlift $ \(MkUnliftIO unlift) -> modifyMVar var $ \olds -> unlift $ fmap swap $ smr olds

mvarUnlift :: MVar s -> Unlift (StateT s)
mvarUnlift var = MkUnlift $ mvarRun var

liftWithMVarStateT :: MonadIO m => (MVar s -> m a) -> StateT s m a
liftWithMVarStateT vma = do
    initialstate <- get
    var <- liftIO $ newMVar initialstate
    r <- lift $ vma var
    finalstate <- liftIO $ takeMVar var
    put finalstate
    return r

readerTUnliftToT :: (MonadTransUnlift t, MonadUnliftIO m) => ReaderT (Unlift t) m a -> t m a
readerTUnliftToT rma = liftWithUnlift $ runReaderT rma

tToReaderTUnlift :: MonadUnliftIO m => t m a -> ReaderT (Unlift t) m a
tToReaderTUnlift tma = do
    MkUnlift unlift <- ask
    lift $ unlift tma

class ( MonadTransConstraint MonadFail t
      , MonadTransConstraint MonadIO t
      , MonadTransConstraint MonadFix t
      , MonadTransConstraint MonadPlus t
      , MonadTransTunnel t
      ) =>
      MonadTransUnlift t where
    liftWithUnlift ::
           forall m r. MonadUnliftIO m
        => (Unlift t -> m r)
        -> t m r
    -- ^ lift with an 'Unlift' that accounts for the transformer's effects (using MVars where necessary)
    getDiscardingUnlift ::
           forall m. Monad m
        => t m (Unlift t)
    -- ^ return an 'Unlift' that discards the transformer's effects (such as state change or output)

-- | Swap two transformers in a transformer stack
evertT ::
       forall ta tb m r. (MonadTransUnlift ta, MonadTransUnlift tb, MonadUnliftIO m)
    => ta (tb m) r
    -> tb (ta m) r
evertT tatbmr =
    case hasTransConstraint @MonadUnliftIO @ta @m of
        Dict -> liftWithUnlift $ \(MkUnlift unlift) -> remonad unlift tatbmr

newtype UnliftIO m = MkUnliftIO
    { runUnliftIO :: forall r. m r -> IO r
    }

identityUnliftIO :: UnliftIO IO
identityUnliftIO = MkUnliftIO id

remonadUnliftIO :: MonadTransTunnel t => (forall a. m1 a -> m2 a) -> UnliftIO (t m2) -> UnliftIO (t m1)
remonadUnliftIO ff (MkUnliftIO r2) = MkUnliftIO $ \m1a -> r2 $ remonad ff m1a

mvarUnliftIO :: MVar s -> UnliftIO (StateT s IO)
mvarUnliftIO var = MkUnliftIO $ mvarRun var

class (MonadFail m, MonadTunnelIO m, MonadFix m) =>
      MonadUnliftIO m where
    liftIOWithUnlift :: forall r. (UnliftIO m -> IO r) -> m r
    -- ^ lift with an 'UnliftIO' that accounts for all transformer effects
    getDiscardingUnliftIO :: m (UnliftIO m)
    -- ^ return an 'UnliftIO' that discards all transformer effects (such as state change or output)

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call $ MkUnliftIO id
    getDiscardingUnliftIO = return $ MkUnliftIO id

instance (MonadTransUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
         MonadUnliftIO (t m) where
    liftIOWithUnlift call =
        liftWithUnlift $ \(MkUnlift tmama) ->
            liftIOWithUnlift $ \(MkUnliftIO maioa) -> call $ MkUnliftIO $ maioa . tmama
    getDiscardingUnliftIO = do
        MkUnlift unlift <- getDiscardingUnlift
        MkUnliftIO unliftIO <- lift getDiscardingUnliftIO
        return $ MkUnliftIO $ unliftIO . unlift

instance MonadTransUnlift t => MonadTransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransUnlift IdentityT where
    liftWithUnlift call = IdentityT $ call identityUnlift
    getDiscardingUnlift = return identityUnlift

instance MonadTransUnlift (ReaderT s) where
    liftWithUnlift call = ReaderT $ \s -> call $ MkUnlift $ \(ReaderT smr) -> smr s
    getDiscardingUnlift = do
        s <- ask
        return $ MkUnlift $ \mr -> runReaderT mr s

instance Monoid s => MonadTransUnlift (WriterT s) where
    liftWithUnlift call = do
        var <- liftIO $ newMVar mempty
        r <-
            lift $
            call $
            MkUnlift $ \(WriterT mrs) -> do
                (r, output) <- mrs
                liftIO $ modifyMVar var $ \oldoutput -> return (mappend oldoutput output, ())
                return r
        totaloutput <- liftIO $ takeMVar var
        tell totaloutput
        return r
    getDiscardingUnlift =
        return $
        MkUnlift $ \mr -> do
            (r, _discarded) <- runWriterT mr
            return r

instance MonadTransUnlift (StateT s) where
    liftWithUnlift call = liftWithMVarStateT (\var -> call $ mvarUnlift var)
    getDiscardingUnlift = do
        s <- get
        return $
            MkUnlift $ \mr -> do
                (r, _discarded) <- runStateT mr s
                return r
