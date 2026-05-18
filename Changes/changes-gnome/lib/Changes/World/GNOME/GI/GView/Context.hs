module Changes.World.GNOME.GI.GView.Context
    ( GTKContext (..)
    , gtkContextSetLock
    )
where

import Import

data GTKContext (ls :: LockState) = MkGTKContext
    { gtkcLock :: SingleThreadLock ls
    , gtkcExit :: IO ()
    , gtkcThrow :: SomeException -> IO ()
    , gtkcExitOnClosed :: View ()
    }

gtkContextSetLock :: SingleThreadLock lsb -> GTKContext lsa -> GTKContext lsb
gtkContextSetLock lock gtc =
    MkGTKContext
        { gtkcLock = lock
        , gtkcExit = gtkcExit gtc
        , gtkcThrow = gtkcThrow gtc
        , gtkcExitOnClosed = gtkcExitOnClosed gtc
        }

instance IsLock GTKContext where
    runLockedIO gtc call =
        runLockedIO (gtkcLock gtc) $ \lock ->
            call $ gtkContextSetLock lock gtc
    runUnlockedIO gtc call =
        runUnlockedIO (gtkcLock gtc) $ \lock ->
            call $ gtkContextSetLock lock gtc
    provideUnlockedIO gtc = do
        lock <- provideUnlockedIO $ gtkcLock gtc
        return $ gtkContextSetLock lock gtc
