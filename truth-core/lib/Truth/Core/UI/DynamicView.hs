module Truth.Core.UI.DynamicView where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.CreateView
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
    -> (Object edit -> [edit] -> StateT dvs IO ())
    -> CreateView edit ()
cvDynamic firstdvs updateCV = do
    stateVar :: MVar dvs <- liftIO $ newMVar firstdvs
    liftLifeCycle $
        lifeCycleClose $ do
            lastdvs <- takeMVar stateVar
            closeDynamicView lastdvs
    let
        update :: Object edit -> [edit] -> IO ()
        update obj edits =
            mvarRun stateVar $ do
                updateCV obj edits
                newdvs <- get
                lift $ for_ (dynamicViewStates newdvs) $ \state -> vsUpdate state obj edits
    cvAddAspect $
        mvarRun stateVar $ do
            dvs <- get
            liftIO $ vsFirstAspect $ dynamicViewFocus dvs
    cvReceiveIOUpdates update
    obj <- cvLiftView viewObject
    liftIO $ update obj []
