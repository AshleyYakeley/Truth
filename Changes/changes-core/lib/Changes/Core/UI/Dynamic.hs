module Changes.Core.UI.Dynamic
    ( DynamicViewState(..)
    , closeDynamicView
    , replaceDynamicView
    , cvDynamic
    , cvSwitch
    , cvOneWholeView
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model
import Changes.Core.Types
import Changes.Core.UI.Selection
import Changes.Core.UI.View.CreateView
import Changes.Core.UI.View.View
import Changes.Debug.Reference

class DynamicViewState (dvs :: Type) where
    dynamicViewStates :: dvs -> [ViewState]

instance DynamicViewState ViewState where
    dynamicViewStates dvs = [dvs]

closeDynamicView :: DynamicViewState dvs => dvs -> IO ()
closeDynamicView dvs = traceBracketIO "closeDynamicView" $ for_ (dynamicViewStates dvs) closeLifeState

replaceDynamicView :: (MonadIO m, DynamicViewState dvs) => m dvs -> StateT dvs m ()
replaceDynamicView getNewDVS = traceBracket "replaceDynamicView" $ do
    oldvs <- get
    liftIO $ closeDynamicView oldvs
    newvs <- lift getNewDVS
    put newvs

cvDynamic ::
       forall dvs update a. (DynamicViewState dvs)
    => Model update
    -> CreateView (dvs, a)
    -> Task ()
    -> (a -> [update] -> StateT dvs View ())
    -> CreateView a
cvDynamic model initCV taskCV recvCV = do
    let
        initBind :: CreateView (MVar dvs, a)
        initBind = do
            (firstdvs, a) <- initCV
            stateVar <- liftIO $ newMVar firstdvs
            lifeCycleClose $ do
                lastdvs <- takeMVar stateVar
                closeDynamicView lastdvs
            return (stateVar, a)
        recvBind :: (MVar dvs, a) -> NonEmpty update -> View ()
        recvBind (stateVar, a) updates = traceBarrier "cvDynamic: recvBind" (mVarRun stateVar) $ recvCV a $ toList updates
    (stateVar, a) <- cvBindModel model Nothing initBind (\_ -> taskCV) recvBind
    liftToLifeCycle $ mVarRun stateVar $ recvCV a []
    return a

cvSwitch :: Model (ROWUpdate (CreateView ())) -> CreateView ()
cvSwitch model = do
    let
        getViewState :: CreateView () -> View ViewState
        getViewState cv = do
            ((), vs) <- getInnerLifeState cv
            return vs
        initVS :: CreateView (ViewState, ())
        initVS = do
            firstspec <- viewRunResource model $ \am -> aModelRead am ReadWhole
            vs <- liftToLifeCycle $ getViewState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (CreateView ())] -> StateT ViewState View ()
        recvVS () updates = hoistIO (traceBracketIO "cvSwitch:update") $ for_ (lastReadOnlyWholeUpdate updates) $ \spec -> replaceDynamicView $ getViewState spec
    cvDynamic model initVS mempty recvVS

data OneWholeViews f =
    MkOneWholeViews (f ())
                    ViewState

instance DynamicViewState (OneWholeViews f) where
    dynamicViewStates (MkOneWholeViews _ vs) = [vs]

cvOneWholeView ::
       forall f update. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> CreateView ())
    -> SelectNotify (f ())
    -> CreateView ()
cvOneWholeView model baseView (MkSelectNotify notifyChange) = do
    let
        readHasOne ::
               forall m. MonadIO m
            => ReaderT ViewContext m (f ())
        readHasOne = viewRunResource model $ \asub -> aModelRead asub ReadHasOne
        getWidgets :: f () -> View (OneWholeViews f)
        getWidgets fu = do
            notifyChange $ return $ Just fu
            ((), vs) <-
                getInnerLifeState $ baseView $ fmap (\() -> mapModel (mustExistOneChangeLens "reference") model) fu
            return $ MkOneWholeViews fu vs
        initVS :: CreateView (OneWholeViews f, ())
        initVS = do
            firstfu <- readHasOne
            vs <- liftToLifeCycle $ getWidgets firstfu
            return (vs, ())
        recvVS :: () -> [FullResultOneUpdate f update] -> StateT (OneWholeViews f) View ()
        recvVS () _ = do
            MkOneWholeViews oldfu vs <- get
            newfu <- lift readHasOne
            case (retrieveOne oldfu, retrieveOne newfu) of
                (SuccessResult (), SuccessResult ()) -> return ()
                (FailureResult _, FailureResult _) -> put $ MkOneWholeViews newfu vs
                _ -> replaceDynamicView $ getWidgets newfu
    cvDynamic model initVS mempty recvVS
