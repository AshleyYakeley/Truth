module Control.Monad.Ology.Data where

import Data.IORef
import Import

-- | borrowed from the lens package
type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

data Param m a = MkParam
    { askD :: m a
    , localD :: forall r. (a -> a) -> m r -> m r
    }

mapParam ::
       forall m a b. Functor m
    => Lens' a b
    -> Param m a
    -> Param m b
mapParam l (MkParam askD localD) = let
    askD' = fmap (\a -> getConst $ l Const a) askD
    localD' :: forall r. (b -> b) -> m r -> m r
    localD' f = localD $ \a -> runIdentity $ l (Identity . f) a
    in MkParam askD' localD'

readerParam :: Monad m => Param (ReaderT r m) r
readerParam = MkParam {askD = ask, localD = local}

data Ref m a = MkRef
    { getD :: m a
    , modifyD :: (a -> a) -> m ()
    }

putD :: Ref m a -> a -> m ()
putD r a = modifyD r $ \_ -> a

mapRef ::
       forall m a b. Monad m
    => Lens' a b
    -> Ref m a
    -> Ref m b
mapRef l (MkRef getD modifyD) = let
    getD' = fmap (\a -> getConst $ l Const a) getD
    modifyD' bb = modifyD $ \a -> runIdentity $ l (Identity . bb) a
    in MkRef getD' modifyD'

stateRef :: Monad m => Ref (StateT s m) s
stateRef = MkRef {getD = get, modifyD = modify}

ioRef :: IORef a -> Ref IO a
ioRef r = MkRef {getD = readIORef r, modifyD = modifyIORef r}

data Prod m a = MkProd
    { tellD :: a -> m ()
    , listenD :: forall r. m r -> m (r, a)
    }

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
