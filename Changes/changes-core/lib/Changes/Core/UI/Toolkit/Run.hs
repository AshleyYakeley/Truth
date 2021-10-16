module Changes.Core.UI.Toolkit.Run where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Core.UI.View.Context
import Changes.Core.UI.View.CreateView
import Changes.Core.UI.View.View

data RunToolkit = MkRunToolkit
    { rtWithLock :: forall a. IO a -> IO a -- ^ run with lock, must not already have it
    , rtWithoutLock :: forall a. IO a -> IO a -- ^ run without lock, must already have it
    , rtUnliftLifeCycle :: MFunction LifeCycle IO -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , rtExit :: IO () -- ^ must already have the lock
    }

-- | Closers will be run at the end of the session. (Lock doesn't matter.)
rtUnliftCreateView :: RunToolkit -> CreateView a -> View a
rtUnliftCreateView uit = hoist $ rtUnliftLifeCycle uit

rtRunView :: MonadUnliftIO m => RunToolkit -> ResourceContext -> ViewT m a -> m a
rtRunView MkRunToolkit {..} vcResourceContext vma = let
    vcWithUILock :: forall a. IO a -> IO a
    vcWithUILock = rtWithLock
    vcWithoutUILock :: forall a. IO a -> IO a
    vcWithoutUILock = rtWithoutLock
    vcExit = rtExit
    in runView MkViewContext {..} vma

nullRunToolkit :: MFunction LifeCycle IO -> RunToolkit
nullRunToolkit rtUnliftLifeCycle = let
    rtWithLock action = action
    rtWithoutLock action = action
    rtExit = return ()
    in MkRunToolkit {..}
