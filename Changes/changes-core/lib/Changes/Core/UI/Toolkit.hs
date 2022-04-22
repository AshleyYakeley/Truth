module Changes.Core.UI.Toolkit where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Core.UI.Toolkit.Run
import Changes.Core.UI.View.CreateView
import Changes.Core.UI.View.View
import Changes.Debug.Reference

data ChangesContext = MkChangesContext
    { ccRunToolkit :: RunToolkit
    , ccExitOnClosed :: forall m. (MonadException m, MonadTunnelIO m) => LifeCycleT m --> LifeCycleT m
    }

ccUnliftLifeCycle :: ChangesContext -> LifeCycle --> IO
ccUnliftLifeCycle cc = rtUnliftLifeCycle $ ccRunToolkit cc

ccRunCreateView :: ChangesContext -> ResourceContext -> CreateView --> LifeCycle
ccRunCreateView cc rc = hoist $ rtRunView (ccRunToolkit cc) rc

ccRunView :: ChangesContext -> ResourceContext -> View --> IO
ccRunView cc rc = rtRunView (ccRunToolkit cc) rc

ccUnliftCreateView :: ChangesContext -> CreateView --> View
ccUnliftCreateView cc = rtUnliftCreateView $ ccRunToolkit cc

nullChangesContext :: (LifeCycle --> IO) -> ChangesContext
nullChangesContext unlift = let
    ccRunToolkit = nullRunToolkit unlift
    ccExitOnClosed = id
    in MkChangesContext {..}

quitOnAllClosed :: MonadIO cm => RunToolkit -> (ChangesContext -> cm r) -> cm r
quitOnAllClosed ccRunToolkit call = do
    (ondone, checkdone) <- liftIO $ lifeCycleOnAllDone $ traceBracket "quit on all closed" $ rtExit ccRunToolkit
    let
        ccExitOnClosed ::
               forall m. (MonadException m, MonadTunnelIO m)
            => LifeCycleT m --> LifeCycleT m
        ccExitOnClosed ma = do
            liftLifeCycle ondone
            ma
    r <- call $ MkChangesContext {..}
    liftIO checkdone
    return r

type ChangesMain = forall a. (ChangesContext -> CreateView a) -> IO a
