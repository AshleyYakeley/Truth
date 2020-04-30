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
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.View.View
import Truth.Core.UI.Window

-- | Closers will be run at the end of the session. (Lock doesn't matter.)
uitUnliftCreateView :: UIToolkit -> CreateView a -> View a
uitUnliftCreateView uit = remonad $ uitUnliftLifeCycle uit

uitRunView :: UIToolkit -> ResourceContext -> ViewT m a -> m a
uitRunView uit rc cv = runView rc (uitWithLock uit) cv (uitGetRequest uit)

uitViewExit :: MonadIO m => UIToolkit -> ViewT m ()
uitViewExit uit = liftIO $ uitExit uit

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
