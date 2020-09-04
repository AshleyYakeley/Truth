module Truth.Core.UI.Toolkit
    ( UIToolkit
    , uitCreateWindow
    , uitGetRequest
    , uitUnliftLifeCycle
    , module Truth.Core.UI.Toolkit
    ) where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.UI.Toolkit.Internal
import Truth.Core.UI.View.Context
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.View.View
import Truth.Core.UI.Window

-- | Closers will be run at the end of the session. (Lock doesn't matter.)
uitUnliftCreateView :: UIToolkit -> CreateView a -> View a
uitUnliftCreateView uit = remonad $ uitUnliftLifeCycle uit

uitRunView :: MonadUnliftIO m => UIToolkit -> ResourceContext -> ViewT m a -> m a
uitRunView MkUIToolkit {..} vcResourceContext vma = let
    vcWithUILock :: forall a. IO a -> IO a
    vcWithUILock = uitWithLock
    vcRequest :: forall t. IOWitness t -> Maybe t
    vcRequest = uitGetRequest
    vcExit = uitExit
    in runView MkViewContext {..} vma

nullUIToolkit :: MFunction LifeCycleIO IO -> UIToolkit
nullUIToolkit uitUnliftLifeCycle = let
    uitWithLock action = action
    uitCreateWindow _ = return nullUIWindow
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
