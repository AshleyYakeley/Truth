module Control.Monad.Ology.Data.Ref where

import Control.Monad.Ology.Data.Param
import Control.Monad.Ology.Data.Prod
import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.StateT
import qualified Control.Monad.ST.Lazy as Lazy
import qualified Control.Monad.ST.Strict as Strict
import Data.IORef
import qualified Data.STRef.Lazy as Lazy
import qualified Data.STRef.Strict as Strict
import Import

data Ref m a = MkRef
    { getD :: m a
    , modifyD :: (a -> a) -> m ()
    }

putD :: Ref m a -> a -> m ()
putD r a = modifyD r $ \_ -> a

restoreD :: (MonadUnliftIO m, MonadException m) => Ref m a -> m --> m
restoreD ref mr = bracket (getD ref) (putD ref) $ \_ -> mr

mapRef ::
       forall m a b. Monad m
    => Lens' a b
    -> Ref m a
    -> Ref m b
mapRef l (MkRef getD modifyD) = let
    getD' = fmap (\a -> getConst $ l Const a) getD
    modifyD' bb = modifyD $ \a -> runIdentity $ l (Identity . bb) a
    in MkRef getD' modifyD'

liftRef :: (MonadTrans t, Monad m) => Ref m --> Ref (t m)
liftRef (MkRef g m) = MkRef (lift g) $ \aa -> lift $ m aa

stateRef :: Monad m => Ref (StateT s m) s
stateRef = MkRef {getD = get, modifyD = modify}

refRunState :: Monad m => Ref m s -> StateT s m --> m
refRunState ref sm = do
    olds <- getD ref
    (a, news) <- runStateT sm olds
    putD ref news
    return a

ioRef :: IORef a -> Ref IO a
ioRef r = MkRef {getD = readIORef r, modifyD = modifyIORef r}

strictSTRef :: Strict.STRef s a -> Ref (Strict.ST s) a
strictSTRef r = MkRef {getD = Strict.readSTRef r, modifyD = Strict.modifySTRef r}

lazySTRef :: Lazy.STRef s a -> Ref (Lazy.ST s) a
lazySTRef r = MkRef {getD = Lazy.readSTRef r, modifyD = Lazy.modifySTRef r}

refParam ::
       forall m a. (MonadUnliftIO m, MonadException m)
    => Ref m a
    -> Param m a
refParam ref@MkRef {..} = let
    askD = getD
    localD :: (a -> a) -> m --> m
    localD aa mr =
        restoreD ref $ do
            modifyD aa
            mr
    in MkParam {..}

refProd ::
       forall m a. (MonadUnliftIO m, MonadException m, Monoid a)
    => Ref m a
    -> Prod m a
refProd ref@MkRef {..} = let
    tellD a = modifyD $ (<>) a
    listenD :: forall r. m r -> m (r, a)
    listenD mr =
        restoreD ref $ do
            putD ref mempty
            r <- mr
            a <- getD
            return (r, a)
    in MkProd {..}
