module Truth.UI.GTK.Closure
    ( makeClosure
    ) where

import Foreign.Ptr
import GI.Gdk
import Shapes

foreign import ccall "wrapper" ioGenFunPtr :: IO () -> IO (FunPtr (IO ()))

makeFunPtr :: IO () -> LifeCycle (FunPtr (IO ()))
makeFunPtr action = do
    fp <- liftIO $ ioGenFunPtr action
    lifeCycleClose $ freeHaskellFunPtr fp
    return fp

makeClosure :: IO () -> LifeCycle Closure
makeClosure action = do
    fp <- makeFunPtr action
    liftIO $ newCClosure fp
