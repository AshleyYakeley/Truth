module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createButton ::
       forall seledit edit. FullEdit edit
    => EditSubject edit
    -> Object edit
    -> CreateView seledit edit Button
createButton subj MkObject {..} =
    cvMakeButton "Create" $
    liftIO $
    runUnliftIO objRun $ do
        edits <- getReplaceEditsFromSubject subj
        pushEdit $ objEdit edits

data OneWholeViews seledit f edit
    = MissingOVS (Limit f)
                 (ViewState seledit (OneWholeEdit f edit) ())
    | PresentOVS (ViewState seledit (OneWholeEdit f edit) ())

instance DynamicViewState (OneWholeViews seledit f edit) where
    type DynamicViewEdit (OneWholeViews seledit f edit) = OneWholeEdit f edit
    type DynamicViewSelEdit (OneWholeViews seledit f edit) = seledit
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]
    dynamicViewFocus (MissingOVS _ vs) = vs
    dynamicViewFocus (PresentOVS vs) = vs

oneWholeView ::
       forall seledit f edit wd. (MonadOne f, FullEdit edit, IsWidget wd)
    => Maybe (Limit f)
    -> (Object (OneWholeEdit f edit) -> CreateView seledit (OneWholeEdit f edit) wd)
    -> GCreateView seledit edit
    -> GCreateView seledit (OneWholeEdit f edit)
oneWholeView mDeleteValue makeEmptywidget baseView = do
    box <- new Box [#orientation := OrientationVertical]
    mDeleteButton <-
        for mDeleteValue $ \(MkLimit deleteValue) -> do
            cvMakeButton "Delete" $ viewObjectPushEdit $ \_ push -> push [SumEditLeft $ MkWholeEdit deleteValue]
    let
        getWidgets :: f () -> View seledit (OneWholeEdit f edit) (OneWholeViews seledit f edit)
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
                            widget <- cvMapEdit (mustExistOneEditLens "object") baseView
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

placeholderLabel :: CreateView seledit edit Label
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
