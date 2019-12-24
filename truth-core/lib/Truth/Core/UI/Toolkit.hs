module Truth.Core.UI.Toolkit where

import Truth.Core.Import
import Truth.Core.UI.Window

data UIToolkit = MkUIToolkit
    { uitWithLock :: forall a. IO a -> IO a -- ^ run with lock, must not already have it
    , uitCreateWindow :: WindowSpec -> LifeCycleIO UIWindow -- ^ must already have lock
    , uitUnliftLifeCycle :: forall a. LifeCycleIO a -> IO a -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , uitExit :: IO () -- ^ must already have the lock
    }

nullUIToolkit :: UIToolkit
nullUIToolkit = let
    uitWithLock action = action
    uitCreateWindow _ = return nullUIWindow
    uitUnliftLifeCycle = runLifeCycle
    uitExit = return ()
    in MkUIToolkit {..}

quitOnWindowsClosed :: UIToolkit -> IO (UIToolkit, IO ())
quitOnWindowsClosed uit = do
    (ondone, checkdone) <- lifeCycleOnAllDone $ uitExit uit
    let
        newCreateWindow :: WindowSpec -> LifeCycleIO UIWindow
        newCreateWindow wspec = do
            ondone
            uitCreateWindow uit wspec
    return (uit {uitCreateWindow = newCreateWindow}, checkdone)

data TruthContext = MkTruthContext
    { tcUIToolkit :: UIToolkit
    }

type TruthMain = forall a. (TruthContext -> LifeCycleIO a) -> IO a
