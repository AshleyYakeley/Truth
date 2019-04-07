module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createButton ::
       forall sel edit. FullEdit edit
    => EditSubject edit
    -> Object edit
    -> CreateView sel edit Button
createButton subj MkObject {..} =
    cvMakeButton "Create" $
    liftIO $
    runTransform objRun $ do
        edits <- getReplaceEditsFromSubject subj
        _ <- pushEdit noEditSource $ objEdit edits
        return ()

data OneWholeViews sel f edit
    = MissingOVS (Limit f)
                 (ViewState sel (OneWholeEdit f edit) ())
    | PresentOVS (ViewState sel (OneWholeEdit f edit) ())

instance DynamicViewState (OneWholeViews sel f edit) where
    type DynamicViewEdit (OneWholeViews sel f edit) = OneWholeEdit f edit
    type DynamicViewSelEdit (OneWholeViews sel f edit) = sel
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]
    dynamicViewFocus (MissingOVS _ vs) = vs
    dynamicViewFocus (PresentOVS vs) = vs

oneWholeView ::
       forall sel f edit wd. (MonadOne f, FullEdit edit, IsWidget wd)
    => Maybe (Limit f)
    -> (Object (OneWholeEdit f edit) -> CreateView sel (OneWholeEdit f edit) wd)
    -> GCreateView sel edit
    -> GCreateView sel (OneWholeEdit f edit)
oneWholeView mDeleteValue makeEmptywidget baseView = do
    box <- new Box [#orientation := OrientationVertical]
    mDeleteButton <-
        for mDeleteValue $ \(MkLimit deleteValue) -> do
            cvMakeButton "Delete" $
                viewObjectPushEdit $ \_ push -> do
                    _ <- push noEditSource [SumEditLeft $ MkWholeEdit deleteValue]
                    return ()
    let
        getWidgets :: f () -> View sel (OneWholeEdit f edit) (OneWholeViews sel f edit)
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
        newfu <- lift $ runTransform unliftIO $ mr ReadHasOne
        case (olddvs, retrieveOne newfu) of
            (PresentOVS _, SuccessResult ()) -> return ()
            (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
            _ -> do
                liftIO $ closeDynamicView olddvs
                newdvs <- liftIO $ runTransform unliftView $ getWidgets newfu
                put newdvs
    toWidget box

placeholderLabel :: CreateView sel edit Label
placeholderLabel = new Label [#label := "Placeholder"]

oneGetView :: GetGView
oneGetView =
    MkGetView $ \getview uispec -> do
        uit <- isUISpec uispec
        return $
            case uit of
                MaybeUISpec mnewval itemspec ->
                    oneWholeView (Just $ MkLimit Nothing) (createButton mnewval) $ getview itemspec
                OneWholeUISpec itemspec -> oneWholeView Nothing (\_ -> placeholderLabel) $ getview itemspec
