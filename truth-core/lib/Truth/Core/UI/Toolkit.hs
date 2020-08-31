module Truth.Core.UI.Toolkit where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.UI.Toolkit.Run
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.View.View

data TruthContext = MkTruthContext
    { tcRunToolkit :: RunToolkit
    , tcExitOnClosed :: forall m. MonadLifeCycleIO m => MFunction m m
    }

tcUnliftLifeCycle :: TruthContext -> MFunction LifeCycleIO IO
tcUnliftLifeCycle tc = rtUnliftLifeCycle $ tcRunToolkit tc

tcRunView :: MonadUnliftIO m => TruthContext -> ResourceContext -> ViewT m a -> m a
tcRunView tc = rtRunView $ tcRunToolkit tc

tcUnliftCreateView :: TruthContext -> CreateView a -> View a
tcUnliftCreateView tc = rtUnliftCreateView $ tcRunToolkit tc

nullTruthContext :: MFunction LifeCycleIO IO -> TruthContext
nullTruthContext unlift = let
    tcRunToolkit = nullRunToolkit unlift
    tcExitOnClosed = id
    in MkTruthContext {..}

quitOnAllClosed :: MonadIO cm => RunToolkit -> (TruthContext -> cm r) -> cm r
quitOnAllClosed tcRunToolkit call = do
    (ondone, checkdone) <- liftIO $ lifeCycleOnAllDone $ rtExit tcRunToolkit
    let
        tcExitOnClosed ::
               forall m. MonadLifeCycleIO m
            => MFunction m m
        tcExitOnClosed ma = do
            liftLifeCycleIO ondone
            ma
    r <- call $ MkTruthContext {..}
    liftIO checkdone
    return r

type TruthMain = forall a. (TruthContext -> CreateView a) -> IO a
