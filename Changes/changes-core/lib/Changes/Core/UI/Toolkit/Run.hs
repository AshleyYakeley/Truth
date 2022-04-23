module Changes.Core.UI.Toolkit.Run where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Core.UI.View.Context
import Changes.Core.UI.View.View

data RunToolkit = MkRunToolkit
    { rtWithLock :: IO --> IO -- ^ run with lock, must not already have it
    , rtWithoutLock :: IO --> IO -- ^ run without lock, must already have it
    , rtRunInMain :: LifeCycle --> IO -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , rtExit :: IO () -- ^ must already have the lock
    }

rtRunView :: RunToolkit -> ResourceContext -> View --> LifeCycle
rtRunView MkRunToolkit {..} vcResourceContext vma = let
    vcWithUILock :: IO --> IO
    vcWithUILock = rtWithLock
    vcWithoutUILock :: IO --> IO
    vcWithoutUILock = rtWithoutLock
    vcRunInMain :: LifeCycle --> IO
    vcRunInMain = rtRunInMain
    vcExit = rtExit
    in runView MkViewContext {..} vma

nullRunToolkit :: LifeCycle --> IO -> RunToolkit
nullRunToolkit rtRunInMain = let
    rtWithLock action = action
    rtWithoutLock action = action
    rtExit = return ()
    in MkRunToolkit {..}
