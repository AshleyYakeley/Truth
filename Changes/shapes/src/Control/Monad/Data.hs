module Control.Monad.Data where

import Data.IORef
import Data.Lens
import Shapes.Import

data Param m a = MkParam
    { askD :: m a
    , localD :: forall r. (a -> a) -> m r -> m r
    }

mapParam ::
       forall m a b. Functor m
    => PureLens a b
    -> Param m a
    -> Param m b
mapParam l (MkParam askD localD) = let
    askD' = fmap (lensGet l) askD
    localD' :: forall r. (b -> b) -> m r -> m r
    localD' f = localD $ pureLensModify l f
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
    => PureLens a b
    -> Ref m a
    -> Ref m b
mapRef l (MkRef getD modifyD) = let
    getD' = fmap (lensGet l) getD
    modifyD' bb = modifyD $ pureLensModify l bb
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
