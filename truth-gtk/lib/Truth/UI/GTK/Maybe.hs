module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

createButton ::
       forall sel update. FullEdit (UpdateEdit update)
    => UpdateSubject update
    -> Object (UpdateEdit update)
    -> CreateView sel update Button
createButton subj (MkRunnableIO objRun MkAnObject {..}) =
    cvMakeButton "Create" $
    liftIO $
    objRun $ do
        edits <- getReplaceEditsFromSubject subj
        _ <- pushEdit noEditSource $ objEdit edits
        return ()

data OneWholeViews sel f
    = MissingOVS (Limit f)
                 (ViewState sel)
    | PresentOVS (ViewState sel)

instance DynamicViewState (OneWholeViews sel f) where
    type DynamicViewSelEdit (OneWholeViews sel f) = sel
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]
    dynamicViewFocus (MissingOVS _ vs) = vs
    dynamicViewFocus (PresentOVS vs) = vs

oneWholeView ::
       forall sel f update wd. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update), IsWidget wd)
    => Maybe (Limit f)
    -> (Object (OneWholeEdit f (UpdateEdit update)) -> CreateView sel (OneWholeUpdate f update) wd)
    -> GCreateView sel update
    -> GCreateView sel (OneWholeUpdate f update)
oneWholeView mDeleteValue makeEmptywidget baseView = do
    box <- new Box [#orientation := OrientationVertical]
    mDeleteButton <-
        for mDeleteValue $ \(MkLimit deleteValue) -> do
            cvMakeButton "Delete" $
                viewObjectPushEdit $ \_ push -> do
                    _ <- push noEditSource [SumEditLeft $ MkWholeReaderEdit deleteValue]
                    return ()
    let
        getWidgets :: f () -> View sel (OneWholeUpdate f update) (OneWholeViews sel f)
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
    cvDynamic firstdvs $ \(MkRunnableIO unliftIO (MkAnObject mr _)) _ -> do
        olddvs <- get
        newfu <- lift $ unliftIO $ mr ReadHasOne
        case (olddvs, retrieveOne newfu) of
            (PresentOVS _, SuccessResult ()) -> return ()
            (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
            _ -> do
                liftIO $ closeDynamicView olddvs
                newdvs <- liftIO $ runWMFunction unliftView $ getWidgets newfu
                put newdvs
    toWidget box

placeholderLabel :: CreateView sel update Label
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
