module Truth.Core.Read.MutableRead where

import Truth.Core.Import

type MutableRead m reader = forall (t :: Type). reader t -> m t

remonadMutableRead :: (forall a. m1 a -> m2 a) -> MutableRead m1 reader -> MutableRead m2 reader
remonadMutableRead mf mr rt = mf (mr rt)

liftMutableRead :: (MonadTrans t, Monad m) => MutableRead m reader -> MutableRead (t m) reader
liftMutableRead = remonadMutableRead lift

newtype MutableReadW m reader = MkMutableReadW
    { unMutableReadW :: MutableRead m reader
    }

stateMutableRead :: Monad m => MutableRead (StateT (MutableReadW m reader) m) reader
stateMutableRead rt = do
    MkMutableReadW mr <- get
    lift $ mr rt

type ReadFunction ra rb = forall m. MonadIO m => MutableRead m ra -> MutableRead m rb

type ReadFunctionT t ra rb = forall m. MonadIO m => MutableRead m ra -> MutableRead (t m) rb

type ReadFunctionF f ra rb = ReadFunctionT (ComposeM f) ra rb
