module Truth.Core.UI.DynamicView where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Specifier

class DynamicViewState (dvs :: Type) where
    type DynamicViewSelEdit dvs :: Type
    dynamicViewStates :: dvs -> [ViewState (DynamicViewSelEdit dvs)]
    dynamicViewFocus :: dvs -> Maybe (ViewState (DynamicViewSelEdit dvs))

instance DynamicViewState (ViewState sel) where
    type DynamicViewSelEdit (ViewState sel) = sel
    dynamicViewStates dvs = [dvs]
    dynamicViewFocus dvs = Just dvs

closeDynamicView :: DynamicViewState dvs => dvs -> IO ()
closeDynamicView dvs = for_ (dynamicViewStates dvs) closeLifeState

replaceDynamicView :: DynamicViewState dvs => IO dvs -> StateT dvs IO ()
replaceDynamicView getNewDVS = do
    oldvs <- get
    liftIO $ closeDynamicView oldvs
    newvs <- liftIO getNewDVS
    put newvs

cvDynamic ::
       forall dvs sel update a. (DynamicViewState dvs, sel ~ DynamicViewSelEdit dvs)
    => OpenSubscriber update
    -> (OpenSubscriber update -> CreateView sel (dvs, a))
    -> Task ()
    -> (a -> [update] -> StateT dvs IO ())
    -> CreateView sel a
cvDynamic sub initCV taskCV recvCV = do
    let
        initBind :: OpenSubscriber update -> CreateView sel (MVar dvs, a)
        initBind rmod = do
            (firstdvs, a) <- initCV rmod
            stateVar <- liftIO $ newMVar firstdvs
            liftLifeCycleIO $
                lifeCycleClose $ do
                    lastdvs <- takeMVar stateVar
                    closeDynamicView lastdvs
            cvAddAspect $
                mVarRun stateVar $ do
                    dvs <- get
                    lift $
                        case dynamicViewFocus dvs of
                            Just vs -> vsFirstAspect vs
                            Nothing -> noAspect
            return (stateVar, a)
        recvBind :: (MVar dvs, a) -> NonEmpty update -> IO ()
        recvBind (stateVar, a) updates = mVarRun stateVar $ recvCV a $ toList updates
    (stateVar, a) <- cvBindSubscriber sub Nothing initBind taskCV recvBind
    liftIO $ mVarRun stateVar $ recvCV a []
    return a
