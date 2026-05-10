module Changes.World.GNOME.GI.GView.Context
    ( GTKContext (..)
    --    , gtkContextToLocked
    --    , gtkContextRelock
    )
where

import Import

data GTKContext (ls :: LockState) = MkGTKContext
    { gtkcLock :: SingleThreadLock ls
    , gtkcExit :: IO ()
    , gtkcThrow :: SomeException -> IO ()
    , gtkcExitOnClosed :: View ()
    }

instance IsLock GTKContext where
    runLockedIO gtc call =
        runLockedIO (gtkcLock gtc) $ \lock' ->
            call
                $ MkGTKContext
                    { gtkcLock = lock'
                    , gtkcExit = gtkcExit gtc
                    , gtkcThrow = gtkcThrow gtc
                    , gtkcExitOnClosed = gtkcExitOnClosed gtc
                    }
    runUnlockedIO gtc call =
        runUnlockedIO (gtkcLock gtc) $ \lock' ->
            call
                $ MkGTKContext
                    { gtkcLock = lock'
                    , gtkcExit = gtkcExit gtc
                    , gtkcThrow = gtkcThrow gtc
                    , gtkcExitOnClosed = gtkcExitOnClosed gtc
                    }
    provideUnlockedIO gtc = do
        lock' <- provideUnlockedIO $ gtkcLock gtc
        return
            $ MkGTKContext
                { gtkcLock = lock'
                , gtkcExit = gtkcExit gtc
                , gtkcThrow = gtkcThrow gtc
                , gtkcExitOnClosed = gtkcExitOnClosed gtc
                }
