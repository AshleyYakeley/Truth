module Truth.Core.UI.DynamicView where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.CreateView
import Truth.Core.UI.View

class DynamicViewState (dvs :: Type) where
    type DynamicViewSelEdit dvs :: Type
    dynamicViewStates :: dvs -> [ViewState (DynamicViewSelEdit dvs)]
    dynamicViewFocus :: dvs -> ViewState (DynamicViewSelEdit dvs)

instance DynamicViewState (ViewState sel) where
    type DynamicViewSelEdit (ViewState sel) = sel
    dynamicViewStates dvs = [dvs]
    dynamicViewFocus dvs = dvs

closeDynamicView :: DynamicViewState dvs => dvs -> IO ()
closeDynamicView dvs = for_ (dynamicViewStates dvs) closeLifeState

cvDynamic ::
       forall dvs sel update. (DynamicViewState dvs, sel ~ DynamicViewSelEdit dvs)
    => dvs
    -> (Object (UpdateEdit update) -> [update] -> StateT dvs IO ())
    -> CreateView sel update ()
cvDynamic firstdvs updateCV = do
    stateVar :: MVar dvs <- liftIO $ newMVar firstdvs
    liftLifeCycleIO $
        lifeCycleClose $ do
            lastdvs <- takeMVar stateVar
            closeDynamicView lastdvs
    let
        update :: Object (UpdateEdit update) -> [update] -> EditSource -> IO ()
        update obj edits _esrc = mVarRun stateVar $ updateCV obj edits
    cvAddAspect $
        mVarRun stateVar $ do
            dvs <- get
            lift $ vsFirstAspect $ dynamicViewFocus dvs
    cvReceiveIOUpdates update
    obj <- cvLiftView viewObject
    liftIO $ update obj [] noEditSource
