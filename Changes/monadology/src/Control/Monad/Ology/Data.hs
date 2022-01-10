module Control.Monad.Ology.Data where

import Control.Monad.Ology.Exception
import Control.Monad.Ology.Function
import Control.Monad.Ology.Trans.Tunnel
import qualified Control.Monad.ST.Lazy as Lazy
import qualified Control.Monad.ST.Strict as Strict
import Data.IORef
import qualified Data.STRef.Lazy as Lazy
import qualified Data.STRef.Strict as Strict
import Import

-- | borrowed from the lens package
type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

data Param m a = MkParam
    { askD :: m a
    , localD :: (a -> a) -> m --> m
    }

withD :: Param m a -> a -> m --> m
withD p a = localD p $ \_ -> a

mapParam ::
       forall m a b. Functor m
    => Lens' a b
    -> Param m a
    -> Param m b
mapParam l (MkParam askD localD) = let
    askD' = fmap (\a -> getConst $ l Const a) askD
    localD' :: (b -> b) -> m --> m
    localD' f = localD $ \a -> runIdentity $ l (Identity . f) a
    in MkParam askD' localD'

liftParam :: (TransTunnel t, Monad m) => Param m --> Param (t m)
liftParam (MkParam a l) = MkParam (lift a) $ \aa -> hoist $ l aa

readerParam :: Monad m => Param (ReaderT r m) r
readerParam = MkParam {askD = ask, localD = local}

refParam ::
       forall m a. MonadBracket m
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

data Ref m a = MkRef
    { getD :: m a
    , modifyD :: (a -> a) -> m ()
    }

putD :: Ref m a -> a -> m ()
putD r a = modifyD r $ \_ -> a

restoreD :: MonadBracket m => Ref m a -> m --> m
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

data Prod m a = MkProd
    { tellD :: a -> m ()
    , listenD :: forall r. m r -> m (r, a)
    }

listen_D :: Functor m => Prod m a -> m () -> m a
listen_D p mu = fmap snd $ listenD p mu

liftProd :: (TransTunnel t, Monad m) => Prod m --> Prod (t m)
liftProd (MkProd t l) =
    MkProd (\a -> lift $ t a) $ \tmr -> tunnel $ \unlift -> fmap (\(tun, a) -> fmap (\r -> (r, a)) tun) $ l $ unlift tmr

writerProd :: Monad m => Prod (WriterT w m) w
writerProd = MkProd {tellD = tell, listenD = listen}

foldProd ::
       forall f m a. (Applicative f, Foldable f, Applicative m)
    => Prod m a
    -> Prod m (f a)
foldProd (MkProd tellD listenD) = let
    tellD' :: f a -> m ()
    tellD' aa = for_ aa tellD
    listenD' :: forall r. m r -> m (r, f a)
    listenD' mr = fmap (\(r, a) -> (r, pure a)) $ listenD mr
    in MkProd tellD' listenD'

refProd ::
       forall m a. (MonadBracket m, Monoid a)
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
