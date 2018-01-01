module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import Graphics.UI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

boxAddShow :: (BoxClass w1, WidgetClass w2) => Packing -> w1 -> w2 -> IO ()
boxAddShow packing w1 w2 = do
    boxPackStart w1 w2 packing 0
    widgetShow w2

containerRemoveDestroy :: (ContainerClass w1, WidgetClass w2) => w1 -> w2 -> IO ()
containerRemoveDestroy w1 w2 = do
    containerRemove w1 w2
    widgetDestroy w2

createButton :: (FullEdit edit) => EditSubject edit -> Object edit -> IO Button
createButton subj MkObject {..} =
    makeButton "Create" $
    objRun $ do
        edits <- getReplaceEditsFromSubject subj
        pushEdit $ objEdit edits

oneWholeView ::
       forall f edit wd. (MonadOne f, FullEdit edit, WidgetClass wd)
    => (forall editb. (FullEdit editb) =>
                          UISpec editb -> UISpec (OneWholeEdit f editb))
    -> Maybe (Limit f)
    -> (Object (OneWholeEdit f edit) -> IO wd)
    -> GCreateView edit
    -> GCreateView (OneWholeEdit f edit)
oneWholeView uispec mDeleteValue makeEmptywidget baseView = do
    box <- liftIO $ vBoxNew False 0
    object <- liftOuter viewObject
    emptyWidget <- liftIO $ makeEmptywidget object
    mDeleteButton <-
        liftOuter $
        liftIOView $ \unlift ->
            for mDeleteValue $ \(MkLimit deleteValue) ->
                makeButton "Delete" $
                unlift $ viewObjectPushEdit $ \push -> push [SumEditLeft $ MkWholeEdit deleteValue]
    let
        getVR :: f () -> View (OneWholeEdit f edit) (f (GViewResult edit))
        getVR fu = for fu $ \() -> mapViewEdit (mustExistOneEditLens "object") $ getCompose $ baseView
        newWidgets :: f (GViewResult edit) -> IO ()
        newWidgets fg =
            case retrieveOne fg of
                FailureResult (MkLimit _) -> do boxAddShow PackGrow box emptyWidget
                SuccessResult vr -> do
                    for_ mDeleteButton (boxAddShow PackNatural box)
                    boxAddShow PackGrow box $ vrWidget vr
    firstfvr <-
        liftOuter $ do
            firstfu <- viewObjectRead $ \mr -> mr ReadHasOne
            getVR firstfu
    liftIO $ newWidgets firstfvr
    stateVar :: MVar (f (GViewResult edit)) <- liftIO $ newMVar firstfvr
    unlift <- liftOuter $ liftIOView return
    let
        update ::
               forall m. MonadUnliftIO m
            => MutableRead m (OneReader f (EditReader edit))
            -> [OneWholeEdit f edit]
            -> m ()
        update mr wedits =
            mvarUnlift stateVar $ do
                oldfvr <- get
                newfu <- lift $ mr ReadHasOne
                case (retrieveOne oldfvr, retrieveOne newfu) of
                    (SuccessResult vr, SuccessResult ()) ->
                        lift $ vrUpdate (mapViewResultEdit (mustExistOneEditLens "object") vr) mr wedits
                    (SuccessResult vr, FailureResult (MkLimit newlf)) -> do
                        liftIO $ containerRemoveDestroy box $ vrWidget vr
                        liftIO $ newWidgets newlf
                        put newlf
                    (FailureResult _, FailureResult (MkLimit newlf)) -> put newlf
                    (FailureResult _, SuccessResult ()) -> do
                        newfvr <- liftIO $ unlift $ getVR newfu
                        for_ newfvr $ \_ -> liftIO $ containerRemove box emptyWidget
                        liftIO $ newWidgets newfvr
                        put newfvr
    createViewAddAspect $
        mvarUnlift stateVar $ do
            fvr <- get
            case getMaybeOne fvr of
                Just vr -> liftIO $ mapAspectSpec uispec $ vrFirstAspect vr
                Nothing -> return Nothing
    createViewReceiveUpdates $ update
    liftOuter $ viewObjectRead $ \mr -> update mr []
    return $ toWidget box

placeholderLabel :: IO Label
placeholderLabel = do
    label <- labelNew (Just "Placeholder")
    return label

oneGetView :: GetGView
oneGetView =
    MkGetView $ \getview uispec -> do
        uit <- isUISpec uispec
        return $
            case uit of
                MkUIMaybe mnewval itemspec ->
                    oneWholeView (uiMaybe Nothing) (Just $ MkLimit Nothing) (createButton mnewval) $ getview itemspec
                MkUIOneWhole itemspec -> oneWholeView uiOneWhole Nothing (\_ -> placeholderLabel) $ getview itemspec
