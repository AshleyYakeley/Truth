module Truth.Core.UI.Toolkit where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.Window

data UIToolkit = MkUIToolkit
    { uitWithLock :: forall a. IO a -> IO a -- ^ run with lock, must not already have it
    , uitCreateWindow :: forall edit. Subscriber edit -> WindowSpec edit -> LifeCycleIO UIWindow -- ^ must already have lock
    , uitUnliftLifeCycle :: forall a. LifeCycleIO a -> IO a -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , uitQuit :: IO () -- ^ must already have the lock
    }

nullUIToolkit :: UIToolkit
nullUIToolkit = let
    uitWithLock action = action
    uitCreateWindow _ _ = return nullUIWindow
    uitUnliftLifeCycle = runLifeCycle
    uitQuit = return ()
    in MkUIToolkit {..}

quitOnWindowsClosed :: UIToolkit -> IO UIToolkit
quitOnWindowsClosed uit = do
    ondone <- lifeCycleOnAllDone $ uitQuit uit
    let
        newCreateWindow :: forall edit. Subscriber edit -> WindowSpec edit -> LifeCycleIO UIWindow
        newCreateWindow sub wspec = do
            ondone
            uitCreateWindow uit sub wspec
    return uit {uitCreateWindow = newCreateWindow}

data TruthContext = MkTruthContext
    { tcUIToolkit :: UIToolkit
    }

type TruthMain = forall a. (TruthContext -> LifeCycleIO a) -> IO a
