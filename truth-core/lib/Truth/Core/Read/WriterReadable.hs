module Truth.Core.Read.WriterReadable where

import Truth.Core.Import
import Truth.Core.Read.Readable
import Truth.Core.Read.SubjectReader

-- | WriterReadable allows copying without reading the whole thing into memory
newtype WriterReadable w reader a = MkWriterReadable
    { unWriterReadable :: forall m. (MonadIO m) =>
                                        MutableRead m reader -> (w -> m ()) -> m a
    }

instance Functor (WriterReadable w reader) where
    fmap ab (MkWriterReadable sma) = MkWriterReadable (\s wm -> fmap ab (sma s wm))

instance Applicative (WriterReadable w reader) where
    pure a = MkWriterReadable $ \_ _ -> pure a
    (MkWriterReadable smab) <*> (MkWriterReadable sma) = MkWriterReadable $ \s wm -> smab s wm <*> sma s wm

instance Monad (WriterReadable w reader) where
    return = pure
    (MkWriterReadable sma) >>= f =
        MkWriterReadable $ \s wm -> do
            a <- sma s wm
            unWriterReadable (f a) s wm

instance MonadIO (WriterReadable w reader) where
    liftIO ioa = MkWriterReadable $ \_ _ -> liftIO ioa

instance IsReadableMonad (WriterReadable w reader) where
    type RMReader (WriterReadable w reader) = reader
    readable rt = MkWriterReadable (\s _ -> s rt)

wrWrite :: w -> WriterReadable w reader ()
wrWrite w = MkWriterReadable $ \_ wm -> wm w

reWriterReadable :: (wa -> wb) -> WriterReadable wa reader a -> WriterReadable wb reader a
reWriterReadable f (MkWriterReadable swma) = MkWriterReadable $ \s wm -> swma s (wm . f)
