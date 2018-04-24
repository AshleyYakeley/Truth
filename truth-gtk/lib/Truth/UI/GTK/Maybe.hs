module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createButton ::
       forall edit. FullEdit edit
    => EditSubject edit
    -> Object edit
    -> CreateView edit Button
createButton subj MkObject {..} =
    cvMakeButton "Create" $
    liftIO $
    runUnliftIO objRun $ do
        edits <- getReplaceEditsFromSubject subj
        pushEdit $ objEdit edits

data OneWholeViews f edit
    = MissingOVS (Limit f)
                 (ViewState (OneWholeEdit f edit) ())
    | PresentOVS (ViewState (OneWholeEdit f edit) ())

instance DynamicViewState (OneWholeViews f edit) where
    type DynamicViewEdit (OneWholeViews f edit) = OneWholeEdit f edit
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]
    dynamicViewFocus (MissingOVS _ vs) = vs
    dynamicViewFocus (PresentOVS vs) = vs

oneWholeView ::
       forall f edit wd. (MonadOne f, FullEdit edit, IsWidget wd)
    => Maybe (Limit f)
    -> (Object (OneWholeEdit f edit) -> CreateView (OneWholeEdit f edit) wd)
    -> GCreateView edit
    -> GCreateView (OneWholeEdit f edit)
oneWholeView mDeleteValue makeEmptywidget baseView = do
    box <- new Box [#orientation := OrientationVertical]
    mDeleteButton <-
        for mDeleteValue $ \(MkLimit deleteValue) -> do
            cvMakeButton "Delete" $ viewObjectPushEdit $ \_ push -> push [SumEditLeft $ MkWholeEdit deleteValue]
    let
        getWidgets :: f () -> View (OneWholeEdit f edit) (OneWholeViews f edit)
        getWidgets fu =
            case retrieveOne fu of
                FailureResult lfx -> do
                    vs <-
                        viewCreateView $ do
                            object <- cvLiftView viewObject
                            w <- makeEmptywidget object
                            lcContainPackStart True box w
                            widgetShow w
                    return $ MissingOVS lfx vs
                SuccessResult () -> do
                    vs <-
                        viewCreateView $ do
                            widget <- mapCreateViewEdit (mustExistOneEditLens "object") baseView
                            for_ mDeleteButton $ \button -> do
                                lcContainPackStart False box button
                                #show button
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ PresentOVS vs
    firstdvs <-
        cvLiftView $ do
            firstfu <- viewObjectRead $ \_ mr -> mr ReadHasOne
            getWidgets firstfu
    unliftView <- cvLiftView askUnliftIO
    cvDynamic firstdvs $ \(MkObject unliftIO mr _) _ -> do
        olddvs <- get
        newfu <- lift $ runUnliftIO unliftIO $ mr ReadHasOne
        case (olddvs, retrieveOne newfu) of
            (PresentOVS _, SuccessResult ()) -> return ()
            (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
            _ -> do
                liftIO $ closeDynamicView olddvs
                newdvs <- liftIO $ runUnliftIO unliftView $ getWidgets newfu
                put newdvs
    toWidget box

placeholderLabel :: CreateView edit Label
placeholderLabel = new Label [#label := "Placeholder"]

oneGetView :: GetGView
oneGetView =
    MkGetView $ \getview uispec -> do
        uit <- isUISpec uispec
        return $
            case uit of
                MkUIMaybe mnewval itemspec ->
                    oneWholeView (Just $ MkLimit Nothing) (createButton mnewval) $ getview itemspec
                MkUIOneWhole itemspec -> oneWholeView Nothing (\_ -> placeholderLabel) $ getview itemspec
