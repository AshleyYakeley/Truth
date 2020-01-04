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

replaceDynamicView :: DynamicViewState dvs => IO dvs -> StateT dvs IO ()
replaceDynamicView getNewDVS = do
    oldvs <- get
    liftIO $ closeDynamicView oldvs
    newvs <- liftIO getNewDVS
    put newvs

cvDynamic ::
       forall dvs sel update. (DynamicViewState dvs, sel ~ DynamicViewSelEdit dvs)
    => OpenSubscriber update
    -> (OpenSubscriber update -> CreateView sel dvs)
    -> ([update] -> StateT dvs IO ())
    -> CreateView sel ()
cvDynamic sub initCV recvCV = do
    let
        initBind :: OpenSubscriber update -> CreateView sel (MVar dvs)
        initBind rmod = do
            firstdvs <- initCV rmod
            stateVar <- liftIO $ newMVar firstdvs
            liftLifeCycleIO $
                lifeCycleClose $ do
                    lastdvs <- takeMVar stateVar
                    closeDynamicView lastdvs
            cvAddAspect $
                mVarRun stateVar $ do
                    dvs <- get
                    lift $ vsFirstAspect $ dynamicViewFocus dvs
            return stateVar
        recvBind :: MVar dvs -> NonEmpty update -> IO ()
        recvBind stateVar updates = mVarRun stateVar $ recvCV $ toList updates
    stateVar <- cvBindSubscriber sub Nothing initBind recvBind
    liftIO $ mVarRun stateVar $ recvCV []
