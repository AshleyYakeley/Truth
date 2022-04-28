module Changes.Core.UI.Toolkit where

import Changes.Core.Import
import Changes.Core.Resource
import Changes.Core.UI.Toolkit.Run
import Changes.Core.UI.View.View
import Changes.Debug.Reference

data ChangesContext = MkChangesContext
    { ccRunToolkit :: RunToolkit
    , ccExitOnClosed :: View --> View
    }

ccUnliftLifeCycle :: ChangesContext -> LifeCycle --> IO
ccUnliftLifeCycle cc = rtRunInMain $ ccRunToolkit cc

ccRunCreateView :: ChangesContext -> ResourceContext -> View --> LifeCycle
ccRunCreateView cc rc = rtRunView (ccRunToolkit cc) rc

ccRunView :: ChangesContext -> ResourceContext -> View --> IO
ccRunView cc rc = ccUnliftLifeCycle cc . ccRunCreateView cc rc

nullChangesContext :: (LifeCycle --> IO) -> ChangesContext
nullChangesContext unlift = let
    ccRunToolkit = nullRunToolkit unlift
    ccExitOnClosed = id
    in MkChangesContext {..}

quitOnAllClosed :: MonadIO cm => RunToolkit -> (ChangesContext -> cm r) -> cm r
quitOnAllClosed ccRunToolkit call = do
    (ondone, checkdone) <- liftIO $ lifeCycleOnAllDone $ traceBracket "quit on all closed" $ rtExit ccRunToolkit
    let
        ccExitOnClosed :: View --> View
        ccExitOnClosed ma = do
            viewLiftLifeCycle ondone
            ma
    r <- call $ MkChangesContext {..}
    liftIO checkdone
    return r

type ChangesMain = forall a. (ChangesContext -> View a) -> IO a
