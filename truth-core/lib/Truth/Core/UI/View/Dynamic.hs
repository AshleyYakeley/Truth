module Truth.Core.UI.View.Dynamic where

import Truth.Core.Import
import Truth.Core.Reference
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.View.View

class DynamicViewState (dvs :: Type) where
    dynamicViewStates :: dvs -> [ViewState]

instance DynamicViewState ViewState where
    dynamicViewStates dvs = [dvs]

closeDynamicView :: DynamicViewState dvs => dvs -> IO ()
closeDynamicView dvs = for_ (dynamicViewStates dvs) closeLifeState

replaceDynamicView :: (MonadIO m, DynamicViewState dvs) => m dvs -> StateT dvs m ()
replaceDynamicView getNewDVS = do
    oldvs <- get
    liftIO $ closeDynamicView oldvs
    newvs <- lift getNewDVS
    put newvs

cvDynamic ::
       forall dvs update a. (DynamicViewState dvs)
    => Model update
    -> (Model update -> CreateView (dvs, a))
    -> Task ()
    -> (a -> [update] -> StateT dvs View ())
    -> CreateView a
cvDynamic sub initCV taskCV recvCV = do
    let
        initBind :: Model update -> CreateView (MVar dvs, a)
        initBind model = do
            (firstdvs, a) <- initCV model
            stateVar <- liftIO $ newMVar firstdvs
            liftLifeCycleIO $
                lifeCycleClose $ do
                    lastdvs <- takeMVar stateVar
                    closeDynamicView lastdvs
            return (stateVar, a)
        recvBind :: (MVar dvs, a) -> NonEmpty update -> View ()
        recvBind (stateVar, a) updates = mVarRun stateVar $ recvCV a $ toList updates
    (stateVar, a) <- cvBindModel sub Nothing initBind taskCV recvBind
    cvLiftView $ mVarRun stateVar $ recvCV a []
    return a
