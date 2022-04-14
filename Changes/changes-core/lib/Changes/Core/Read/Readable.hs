module Changes.Core.Read.Readable where

import Changes.Core.Import

type Readable m reader = forall (t :: Type). reader t -> m t

hoistReadable :: forall m1 m2 reader. (m1 --> m2) -> Readable m1 reader -> Readable m2 reader
hoistReadable mf mr rt = mf (mr rt)

liftReadable ::
       forall t m reader. (MonadTrans t, Monad m)
    => Readable m reader
    -> Readable (t m) reader
liftReadable = hoistReadable lift

stackLiftReadable ::
       forall tt m reader. (MonadTransStackUnlift tt, Monad m)
    => Readable m reader
    -> Readable (ApplyStack tt m) reader
stackLiftReadable = hoistReadable @m @(ApplyStack tt m) $ stackLift @tt

newtype ReadableW m reader = MkReadableW
    { unReadableW :: Readable m reader
    }

stateReadable :: Monad m => Readable (StateT (ReadableW m reader) m) reader
stateReadable rt = do
    MkReadableW mr <- get
    lift $ mr rt

type ReadFunction ra rb = forall m. MonadIO m => Readable m ra -> Readable m rb

type ReadFunctionT t ra rb = forall m. MonadIO m => Readable m ra -> Readable (t m) rb

type ReadFunctionTT (tt :: [TransKind]) ra rb = forall m. MonadIO m => Readable m ra -> Readable (ApplyStack tt m) rb

type ReadFunctionF f ra rb = ReadFunctionT (ComposeInner f) ra rb
