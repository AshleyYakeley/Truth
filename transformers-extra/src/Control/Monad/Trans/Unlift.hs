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
import Control.Monad.Trans.Transform
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Writer
import Data.Constraint
import Data.Kind
import Data.Tuple
import Prelude

class ( MonadTransConstraint MonadFail t
      , MonadTransConstraint MonadIO t
      , MonadTransConstraint MonadFix t
      , MonadTransSemiTunnel t
      ) => MonadTransSemiUnlift t where
    liftWithSemiUnlift ::
           forall m r. MonadUnliftIO m
        => (Transform (t m) m -> m r)
        -> t m r
    -- ^ lift with a 'Transform' that accounts for the transformer's effects (using MVars where necessary)
    default liftWithSemiUnlift ::
        MonadTransUnlift t => forall m r. MonadUnliftIO m => (Transform (t m) m -> m r) -> t m r
    liftWithSemiUnlift call = liftWithUnlift $ \unlift -> call $ unliftTransform unlift
    getDiscardingSemiUnlift ::
           forall m. MonadUnliftIO m
        => t m (Transform (t m) m)
    -- ^ return a 'Transform' that discards the transformer's effects (such as state change or output)
    default getDiscardingSemiUnlift :: forall m. (MonadTransUnlift t, MonadUnliftIO m) => t m (Transform (t m) m)
    getDiscardingSemiUnlift =
        case hasTransConstraint @Monad @t @m of
            Dict -> fmap unliftTransform getDiscardingUnlift

newtype Unlift (t :: (Type -> Type) -> Type -> Type) = MkUnlift
    { runUnlift :: forall (m :: Type -> Type) (r :: Type). MonadUnliftIO m => t m r -> m r
    }

unliftTransform :: MonadUnliftIO m => Unlift t -> Transform (t m) m
unliftTransform (MkUnlift unlift) = MkTransform unlift

identityUnlift :: Unlift IdentityT
identityUnlift = MkUnlift runIdentityT

mvarRun :: MonadUnliftIO m => MVar s -> StateT s m r -> m r
mvarRun var (StateT smr) =
    liftIOWithUnlift $ \(MkTransform unlift) -> modifyMVar var $ \olds -> unlift $ fmap swap $ smr olds

mvarUnlift :: MVar s -> Unlift (StateT s)
mvarUnlift var = MkUnlift $ mvarRun var

liftStateT :: (Traversable f, Applicative m) => StateT s m a -> StateT (f s) m (f a)
liftStateT (StateT smas) = StateT $ \fs -> fmap (\fas -> (fmap fst fas, fmap snd fas)) $ traverse smas fs

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

class (MonadTransConstraint MonadPlus t, MonadTransTunnel t, MonadTransSemiUnlift t) => MonadTransUnlift t where
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
commuteT ::
       forall ta tb m r. (MonadTransUnlift ta, MonadTransUnlift tb, MonadUnliftIO m)
    => ta (tb m) r
    -> tb (ta m) r
commuteT tatbmr =
    case hasTransConstraint @MonadUnliftIO @ta @m of
        Dict -> liftWithUnlift $ \(MkUnlift unlift) -> remonad' unlift tatbmr

type UnliftIO m = Transform m IO

mvarUnliftIO :: MVar s -> UnliftIO (StateT s IO)
mvarUnliftIO var = MkTransform $ mvarRun var

composeUnliftTransform :: (MonadTransUnlift t, MonadUnliftIO m) => Unlift t -> Transform m n -> Transform (t m) n
composeUnliftTransform (MkUnlift rt) (MkTransform rm) = MkTransform $ \tma -> rm $ rt tma

composeUnliftTransformCommute ::
       (MonadTransUnlift t, MonadUnliftIO m, MonadUnliftIO n) => Unlift t -> Transform m n -> Transform (t m) n
composeUnliftTransformCommute (MkUnlift rt) (MkTransform rm) = MkTransform $ \tma -> rt $ remonad rm tma

class (MonadFail m, MonadTunnelIO m, MonadFix m) => MonadUnliftIO m where
    liftIOWithUnlift :: forall r. (UnliftIO m -> IO r) -> m r
    -- ^ lift with an 'UnliftIO' that accounts for all transformer effects
    getDiscardingUnliftIO :: m (UnliftIO m)
    -- ^ return an 'UnliftIO' that discards all transformer effects (such as state change or output)

instance MonadUnliftIO IO where
    liftIOWithUnlift call = call $ MkTransform id
    getDiscardingUnliftIO = return $ MkTransform id

instance (MonadTransSemiUnlift t, MonadUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
             MonadUnliftIO (t m) where
    liftIOWithUnlift call =
        liftWithSemiUnlift $ \(MkTransform tmama) ->
            liftIOWithUnlift $ \(MkTransform maioa) -> call $ MkTransform $ maioa . tmama
    getDiscardingUnliftIO = do
        MkTransform unlift <- getDiscardingSemiUnlift
        MkTransform unliftIO <- lift getDiscardingUnliftIO
        return $ MkTransform $ unliftIO . unlift

instance MonadTransSemiUnlift t => MonadTransConstraint MonadUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransSemiUnlift IdentityT

instance MonadTransUnlift IdentityT where
    liftWithUnlift call = IdentityT $ call identityUnlift
    getDiscardingUnlift = return identityUnlift

instance MonadTransSemiUnlift (ReaderT s)

instance MonadTransUnlift (ReaderT s) where
    liftWithUnlift call = ReaderT $ \s -> call $ MkUnlift $ \(ReaderT smr) -> smr s
    getDiscardingUnlift = do
        s <- ask
        return $ MkUnlift $ \mr -> runReaderT mr s

instance Monoid s => MonadTransSemiUnlift (WriterT s)

writerDiscardingUnlift :: Unlift (WriterT s)
writerDiscardingUnlift =
    MkUnlift $ \mr -> do
        (r, _discarded) <- runWriterT mr
        return r

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
    getDiscardingUnlift = return writerDiscardingUnlift

instance MonadTransSemiUnlift (StateT s)

stateDiscardingUnlift :: s -> Unlift (StateT s)
stateDiscardingUnlift s =
    MkUnlift $ \mr -> do
        (r, _discarded) <- runStateT mr s
        return r

instance MonadTransUnlift (StateT s) where
    liftWithUnlift call = liftWithMVarStateT (\var -> call $ mvarUnlift var)
    getDiscardingUnlift = do
        s <- get
        return $ stateDiscardingUnlift s
