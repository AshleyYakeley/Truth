module Control.FixIO where

import Shapes.Import
import Shapes.Unsafe

mfixIO :: MonadIO m => (a -> m a) -> m a
mfixIO ama = do
    ref <- liftIO $ newEmptyMVar
    olda <- liftIO $ unsafeInterleaveIO $ takeMVar ref
    newa <- ama olda
    liftIO $ putMVar ref newa
    return newa
