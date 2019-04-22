module Truth.Core.UI.DynamicView where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.CreateView
import Truth.Core.UI.View

class DynamicViewState (dvs :: Type) where
    type DynamicViewSelEdit dvs :: Type
    type DynamicViewEdit dvs :: Type
    dynamicViewStates :: dvs -> [ViewState (DynamicViewSelEdit dvs) (DynamicViewEdit dvs)]
    dynamicViewFocus :: dvs -> ViewState (DynamicViewSelEdit dvs) (DynamicViewEdit dvs)

instance DynamicViewState (ViewState sel edit) where
    type DynamicViewSelEdit (ViewState sel edit) = sel
    type DynamicViewEdit (ViewState sel edit) = edit
    dynamicViewStates dvs = [dvs]
    dynamicViewFocus dvs = dvs

closeDynamicView :: DynamicViewState dvs => dvs -> IO ()
closeDynamicView dvs = for_ (dynamicViewStates dvs) closeLifeState

cvDynamic ::
       forall dvs sel edit. (DynamicViewState dvs, sel ~ DynamicViewSelEdit dvs, edit ~ DynamicViewEdit dvs)
    => dvs
    -> (Object edit -> [edit] -> StateT dvs IO ())
    -> CreateView sel edit ()
cvDynamic firstdvs updateCV = do
    stateVar :: MVar dvs <- liftIO $ newMVar firstdvs
    liftLifeCycleIO $
        lifeCycleClose $ do
            lastdvs <- takeMVar stateVar
            closeDynamicView lastdvs
    let
        update :: Object edit -> [edit] -> EditSource -> IO ()
        update obj edits esrc =
            mvarRun stateVar $ do
                updateCV obj edits
                newdvs <- get
                lift $ for_ (dynamicViewStates newdvs) $ \state -> vsUpdate state obj edits $ MkEditContext esrc False
    cvAddAspect $
        mvarRun stateVar $ do
            dvs <- get
            liftIO $ vsFirstAspect $ dynamicViewFocus dvs
    cvReceiveIOUpdates update
    obj <- cvLiftView viewObject
    liftIO $ update obj [] noEditSource
