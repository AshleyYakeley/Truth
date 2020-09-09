module Changes.Core.UI.Toolkit where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Core.UI.Toolkit.Run
import Changes.Core.UI.View.CreateView
import Changes.Core.UI.View.View

data ChangesContext = MkChangesContext
    { tcRunToolkit :: RunToolkit
    , tcExitOnClosed :: forall m. MonadLifeCycleIO m => MFunction m m
    }

tcUnliftLifeCycle :: ChangesContext -> MFunction LifeCycleIO IO
tcUnliftLifeCycle tc = rtUnliftLifeCycle $ tcRunToolkit tc

tcRunView :: MonadUnliftIO m => ChangesContext -> ResourceContext -> ViewT m a -> m a
tcRunView tc = rtRunView $ tcRunToolkit tc

tcUnliftCreateView :: ChangesContext -> CreateView a -> View a
tcUnliftCreateView tc = rtUnliftCreateView $ tcRunToolkit tc

nullChangesContext :: MFunction LifeCycleIO IO -> ChangesContext
nullChangesContext unlift = let
    tcRunToolkit = nullRunToolkit unlift
    tcExitOnClosed = id
    in MkChangesContext {..}

quitOnAllClosed :: MonadIO cm => RunToolkit -> (ChangesContext -> cm r) -> cm r
quitOnAllClosed tcRunToolkit call = do
    (ondone, checkdone) <- liftIO $ lifeCycleOnAllDone $ rtExit tcRunToolkit
    let
        tcExitOnClosed ::
               forall m. MonadLifeCycleIO m
            => MFunction m m
        tcExitOnClosed ma = do
            liftLifeCycleIO ondone
            ma
    r <- call $ MkChangesContext {..}
    liftIO checkdone
    return r

type ChangesMain = forall a. (ChangesContext -> CreateView a) -> IO a
