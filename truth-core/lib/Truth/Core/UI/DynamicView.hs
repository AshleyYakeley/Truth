module Truth.Core.UI.DynamicView where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.View

class DynamicViewState (dvs :: *) where
    type DynamicViewEdit dvs :: *
    dynamicViewStates :: dvs -> [ViewState (DynamicViewEdit dvs) ()]
    dynamicViewFocus :: dvs -> ViewState (DynamicViewEdit dvs) ()

instance DynamicViewState (ViewState edit ()) where
    type DynamicViewEdit (ViewState edit ()) = edit
    dynamicViewStates dvs = [dvs]
    dynamicViewFocus dvs = dvs

closeDynamicView :: DynamicViewState dvs => dvs -> IO ()
closeDynamicView dvs = for_ (dynamicViewStates dvs) closeLifeState

cvDynamic ::
       forall dvs edit. (DynamicViewState dvs, edit ~ DynamicViewEdit dvs)
    => dvs
    -> (UnliftIO (View edit) -> ReceiveUpdatesT (StateT dvs) edit)
    -> CreateView edit ()
cvDynamic firstdvs updateCV = do
    stateVar :: MVar dvs <- liftIO $ newMVar firstdvs
    liftLifeCycle $
        lifeCycleClose $ do
            lastdvs <- takeMVar stateVar
            closeDynamicView lastdvs
    let
        update :: UnliftIO (View edit) -> ReceiveUpdates edit
        update unlift mr edits =
            mvarRun stateVar $ do
                updateCV unlift mr edits
                newdvs <- get
                lift $ for_ (dynamicViewStates newdvs) $ \(state, _) -> vrUpdate state mr edits
    cvAddAspect $
        mvarRun stateVar $ do
            dvs <- get
            liftIO $ vrFirstAspect $ fst $ dynamicViewFocus dvs
    cvReceiveUpdates update
    cvLiftView $ viewObjectRead $ \unlift mr -> update unlift mr []
