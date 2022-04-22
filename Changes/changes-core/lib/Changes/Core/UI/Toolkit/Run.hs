module Changes.Core.UI.Toolkit.Run where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Core.UI.View.Context
import Changes.Core.UI.View.CreateView
import Changes.Core.UI.View.View

data RunToolkit = MkRunToolkit
    { rtWithLock :: IO --> IO -- ^ run with lock, must not already have it
    , rtWithoutLock :: IO --> IO -- ^ run without lock, must already have it
    , rtUnliftLifeCycle :: LifeCycle --> IO -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , rtExit :: IO () -- ^ must already have the lock
    }

-- | Closers will be run at the end of the session. (Lock doesn't matter.)
rtUnliftCreateView :: RunToolkit -> CreateView a -> View a
rtUnliftCreateView uit cv = hoist (rtUnliftLifeCycle uit) $ commuteT cv

rtRunView :: MonadUnliftIO m => RunToolkit -> ResourceContext -> ViewT m a -> m a
rtRunView MkRunToolkit {..} vcResourceContext vma = let
    vcWithUILock :: IO --> IO
    vcWithUILock = rtWithLock
    vcWithoutUILock :: IO --> IO
    vcWithoutUILock = rtWithoutLock
    vcExit = rtExit
    in runView MkViewContext {..} vma

nullRunToolkit :: LifeCycle --> IO -> RunToolkit
nullRunToolkit rtUnliftLifeCycle = let
    rtWithLock action = action
    rtWithoutLock action = action
    rtExit = return ()
    in MkRunToolkit {..}
