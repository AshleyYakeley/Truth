module Truth.Core.UI.Toolkit.Run where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.UI.View.Context
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.View.View

data RunToolkit = MkRunToolkit
    { rtWithLock :: forall a. IO a -> IO a -- ^ run with lock, must not already have it
    , rtUnliftLifeCycle :: MFunction LifeCycleIO IO -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , rtExit :: IO () -- ^ must already have the lock
    }

-- | Closers will be run at the end of the session. (Lock doesn't matter.)
rtUnliftCreateView :: RunToolkit -> CreateView a -> View a
rtUnliftCreateView uit = remonad $ rtUnliftLifeCycle uit

rtRunView :: MonadUnliftIO m => RunToolkit -> ResourceContext -> ViewT m a -> m a
rtRunView MkRunToolkit {..} vcResourceContext vma = let
    vcWithUILock :: forall a. IO a -> IO a
    vcWithUILock = rtWithLock
    vcExit = rtExit
    in runView MkViewContext {..} vma

nullRunToolkit :: MFunction LifeCycleIO IO -> RunToolkit
nullRunToolkit rtUnliftLifeCycle = let
    rtWithLock action = action
    rtExit = return ()
    in MkRunToolkit {..}
