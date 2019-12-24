module Truth.Core.UI.DynamicView where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.CreateView

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
    => Subscriber update
    -> dvs
    -> ([update] -> StateT dvs IO ())
    -> CreateView sel ()
cvDynamic sub firstdvs recvCV = do
    stateVar :: MVar dvs <- liftIO $ newMVar firstdvs
    liftLifeCycleIO $
        lifeCycleClose $ do
            lastdvs <- takeMVar stateVar
            closeDynamicView lastdvs
    let
        recv :: [update] -> EditSource -> IO ()
        recv updates _esrc = mVarRun stateVar $ recvCV updates
    cvAddAspect $
        mVarRun stateVar $ do
            dvs <- get
            lift $ vsFirstAspect $ dynamicViewFocus dvs
    cvReceiveIOUpdates sub $ \uu -> recv $ toList uu
    liftIO $ recv [] noEditSource
