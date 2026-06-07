module Changes.World.GNOME.GI.GView.Context
    ( GTKContext (..)
    )
where

import Import

data GTKContext = MkGTKContext
    { gtkcLock :: SingleThreadLock
    , gtkcExit :: Result (Exc IO) () -> IO ()
    , gtkcWaitForExit :: IO (Result (Exc IO) ())
    , gtkcExitOnClosed :: View ()
    }

instance IsLock GTKContext where
    runLockedIO gtc = runLockedIO $ gtkcLock gtc
    runUnlockedIO gtc = runUnlockedIO $ gtkcLock gtc
