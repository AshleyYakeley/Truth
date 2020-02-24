module Truth.Core.UI.Toolkit where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.View.View
import Truth.Core.UI.Window

data UIToolkit = MkUIToolkit
    { uitWithLock :: forall a. IO a -> IO a -- ^ run with lock, must not already have it
    , uitCreateWindow :: WindowSpec -> CreateView UIWindow -- ^ must already have lock
    , uitUnliftLifeCycle :: forall a. LifeCycleIO a -> IO a -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , uitGetRequest :: forall t. IOWitness t -> Maybe t
    , uitExit :: IO () -- ^ must already have the lock
    }

-- | Closers will be run at the end of the session. (Lock doesn't matter.)
uitUnliftCreateView :: UIToolkit -> CreateView a -> View a
uitUnliftCreateView uit = cvUnliftView $ uitUnliftLifeCycle uit

uitRunView :: UIToolkit -> ResourceContext -> View a -> IO a
uitRunView uit rc cv = runView rc (uitWithLock uit) cv (uitGetRequest uit)

uitRunCreateView :: UIToolkit -> ResourceContext -> CreateView a -> LifeCycleIO a
uitRunCreateView uit rc cv = runCreateView rc (uitWithLock uit) cv (uitGetRequest uit)

nullUIToolkit :: UIToolkit
nullUIToolkit = let
    uitWithLock action = action
    uitCreateWindow _ = return nullUIWindow
    uitUnliftLifeCycle = runLifeCycle
    uitGetRequest _ = Nothing
    uitExit = return ()
    in MkUIToolkit {..}

quitOnWindowsClosed :: UIToolkit -> IO (UIToolkit, IO ())
quitOnWindowsClosed uit = do
    (ondone, checkdone) <- lifeCycleOnAllDone $ uitExit uit
    let
        newCreateWindow :: WindowSpec -> CreateView UIWindow
        newCreateWindow wspec = do
            liftLifeCycleIO ondone
            uitCreateWindow uit wspec
    return (uit {uitCreateWindow = newCreateWindow}, checkdone)

data TruthContext = MkTruthContext
    { tcUIToolkit :: UIToolkit
    }

type TruthMain = forall a. (TruthContext -> CreateView a) -> IO a
