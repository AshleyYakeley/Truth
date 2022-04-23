module Changes.Core.UI.Dynamic
    ( DynamicViewState(..)
    , closeDynamicView
    , replaceDynamicView
    , viewDynamic
    , viewSwitch
    , viewInnerWholeView
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model
import Changes.Core.Types
import Changes.Core.UI.Selection
import Changes.Core.UI.View.View

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

viewDynamic ::
       forall dvs update a. (DynamicViewState dvs)
    => Model update
    -> View (dvs, a)
    -> Task ()
    -> (a -> [update] -> StateT dvs View ())
    -> View a
viewDynamic model initCV taskCV recvCV = do
    let
        initBind :: View (MVar dvs, a)
        initBind = do
            (firstdvs, a) <- initCV
            stateVar <- liftIO $ newMVar firstdvs
            viewCloserIO $ do
                lastdvs <- takeMVar stateVar
                closeDynamicView lastdvs
            return (stateVar, a)
        recvBind :: (MVar dvs, a) -> NonEmpty update -> View ()
        recvBind (stateVar, a) updates = mVarRun stateVar $ recvCV a $ toList updates
    (stateVar, a) <- viewBindModel model Nothing initBind (\_ -> taskCV) recvBind
    mVarRun stateVar $ recvCV a []
    return a

viewSwitch :: Model (ROWUpdate (View ())) -> View ()
viewSwitch model = do
    let
        getViewState :: View () -> View ViewState
        getViewState cv = do
            ((), vs) <- viewGetState cv
            return vs
        initVS :: View (ViewState, ())
        initVS = do
            firstspec <- viewRunResource model $ \am -> aModelRead am ReadWhole
            vs <- getViewState firstspec
            return (vs, ())
        recvVS :: () -> [ROWUpdate (View ())] -> StateT ViewState View ()
        recvVS () updates = for_ (lastReadOnlyWholeUpdate updates) $ \spec -> replaceDynamicView $ getViewState spec
    viewDynamic model initVS mempty recvVS

data OneWholeViews f =
    MkOneWholeViews (f ())
                    ViewState

instance DynamicViewState (OneWholeViews f) where
    dynamicViewStates (MkOneWholeViews _ vs) = [vs]

viewInnerWholeView ::
       forall f update. (MonadInner f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> View ())
    -> SelectNotify (f ())
    -> View ()
viewInnerWholeView model baseView (MkSelectNotify notifyChange) = do
    let
        readInner :: View (f ())
        readInner = viewRunResource model $ \asub -> aModelRead asub ReadHasOne
        getWidgets :: f () -> View (OneWholeViews f)
        getWidgets fu = do
            notifyChange $ return $ Just fu
            ((), vs) <- viewGetState $ baseView $ fmap (\() -> mapModel (mustExistOneChangeLens "reference") model) fu
            return $ MkOneWholeViews fu vs
        initVS :: View (OneWholeViews f, ())
        initVS = do
            firstfu <- readInner
            vs <- getWidgets firstfu
            return (vs, ())
        recvVS :: () -> [FullResultOneUpdate f update] -> StateT (OneWholeViews f) View ()
        recvVS () _ = do
            MkOneWholeViews oldfu vs <- get
            newfu <- lift readInner
            case (retrieveInner oldfu, retrieveInner newfu) of
                (SuccessResult (), SuccessResult ()) -> return ()
                (FailureResult _, FailureResult _) -> put $ MkOneWholeViews newfu vs
                _ -> replaceDynamicView $ getWidgets newfu
    viewDynamic model initVS mempty recvVS
