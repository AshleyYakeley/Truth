module Changes.Core.UI.Toolkit where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Core.UI.Toolkit.Run
import Changes.Core.UI.View.CreateView
import Changes.Core.UI.View.View

data ChangesContext = MkChangesContext
    { ccRunToolkit :: RunToolkit
    , ccExitOnClosed :: forall m. MonadLifeCycleIO m => MFunction m m
    }

ccUnliftLifeCycle :: ChangesContext -> MFunction LifeCycle IO
ccUnliftLifeCycle cc = rtUnliftLifeCycle $ ccRunToolkit cc

ccRunView :: MonadUnliftIO m => ChangesContext -> ResourceContext -> ViewT m a -> m a
ccRunView cc = rtRunView $ ccRunToolkit cc

ccUnliftCreateView :: ChangesContext -> CreateView a -> View a
ccUnliftCreateView cc = rtUnliftCreateView $ ccRunToolkit cc

nullChangesContext :: MFunction LifeCycle IO -> ChangesContext
nullChangesContext unlift = let
    ccRunToolkit = nullRunToolkit unlift
    ccExitOnClosed = id
    in MkChangesContext {..}

quitOnAllClosed :: MonadIO cm => RunToolkit -> (ChangesContext -> cm r) -> cm r
quitOnAllClosed ccRunToolkit call = do
    (ondone, checkdone) <- liftIO $ lifeCycleOnAllDone $ rtExit ccRunToolkit
    let
        ccExitOnClosed ::
               forall m. MonadLifeCycleIO m
            => MFunction m m
        ccExitOnClosed ma = do
            liftLifeCycle ondone
            ma
    r <- call $ MkChangesContext {..}
    liftIO checkdone
    return r

type ChangesMain = forall a. (ChangesContext -> CreateView a) -> IO a
