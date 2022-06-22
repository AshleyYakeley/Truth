module Changes.Core.UI.Dynamic
    ( replaceDynamicView
    , viewDynamic
    , viewInnerWholeView
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model
import Changes.Core.Types
import Changes.Core.UI.Selection
import Changes.Core.UI.View.View
import Changes.Debug

replaceDynamicView :: MonadIO m => m dvs -> (dvs -> IO ViewState) -> StateT dvs m ()
replaceDynamicView getNewDVS tovsCV = do
    olddvs <- get
    liftIO $ do
        oldvs <- tovsCV olddvs
        closeLifeState oldvs
    newdvs <- lift getNewDVS
    put newdvs

viewDynamic ::
       forall dvs update a.
       Model update
    -> View (dvs, a)
    -> (dvs -> IO ViewState)
    -> Task ()
    -> (a -> [update] -> StateT dvs View ())
    -> View a
viewDynamic model initCV tovsCV taskCV recvCV = do
    let
        initBind :: View (MVar dvs, a)
        initBind = do
            (firstdvs, a) <- initCV
            stateVar <- liftIO $ newMVar firstdvs
            viewOnCloseIO $ do
                lastdvs <- takeMVar stateVar
                vs <- tovsCV lastdvs
                traceBracket "viewDynamic.close" $ closeLifeState vs
            return (stateVar, a)
        recvBind :: (MVar dvs, a) -> NonEmpty update -> View ()
        recvBind (stateVar, a) updates = mVarRun stateVar $ recvCV a $ toList updates
    (stateVar, a) <- viewBindModel model Nothing initBind (\_ -> taskCV) recvBind
    mVarRun stateVar $ recvCV a []
    return a

data OneWholeViews f =
    MkOneWholeViews (f ())
                    ViewState

viewInnerWholeView ::
       forall f update. (MonadInner f, IsUpdate update, FullEdit (UpdateEdit update))
    => Model (FullResultOneUpdate f update)
    -> (f (Model update) -> View ())
    -> SelectNotify (f ())
    -> View ()
viewInnerWholeView model baseView (MkSelectNotify notifyChange) = let
    readInner :: View (f ())
    readInner = viewRunResource model $ \asub -> aModelRead asub ReadHasOne
    getWidgets :: f () -> View (OneWholeViews f)
    getWidgets fu = do
        notifyChange $ return $ Just fu
        ((), vs) <- viewGetViewState $ baseView $ fmap (\() -> mapModel (mustExistOneChangeLens "reference") model) fu
        return $ MkOneWholeViews fu vs
    initVS :: View (OneWholeViews f, ())
    initVS = do
        firstfu <- readInner
        vs <- getWidgets firstfu
        return (vs, ())
    tocvVS :: OneWholeViews f -> IO ViewState
    tocvVS (MkOneWholeViews _ vs) = return vs
    recvVS :: () -> [FullResultOneUpdate f update] -> StateT (OneWholeViews f) View ()
    recvVS () _ = do
        MkOneWholeViews oldfu vs <- get
        newfu <- lift readInner
        case (retrieveInner oldfu, retrieveInner newfu) of
            (SuccessResult (), SuccessResult ()) -> return ()
            (FailureResult _, FailureResult _) -> put $ MkOneWholeViews newfu vs
            _ -> replaceDynamicView (getWidgets newfu) tocvVS
    in viewDynamic model initVS tocvVS mempty recvVS
