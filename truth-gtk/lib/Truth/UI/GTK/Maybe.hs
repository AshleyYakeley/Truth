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
    -> (NonEmpty edit -> View sel ())
    -> CreateView sel Button
createButton subj action =
    cvMakeButton "Create" $ do
        edits <- getReplaceEditsFromSubject subj
        case nonEmpty edits of
            Nothing -> return ()
            Just edits' -> action edits'

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
    -> Subscriber (OneWholeUpdate f update)
    -> ((NonEmpty (OneWholeEdit f (UpdateEdit update)) -> View sel ()) -> CreateView sel wd)
    -> (Subscriber update -> GCreateView sel)
    -> GCreateView sel
oneWholeView mDeleteValue sub makeEmptywidget baseView =
    runResource sub $ \run asub -> do
        box <- new Box [#orientation := OrientationVertical]
        mDeleteButton <-
            for mDeleteValue $ \(MkLimit deleteValue) -> do
                cvMakeButton "Delete" $
                    liftIO $
                    run $ do
                        _ <- pushEdit noEditSource $ subEdit asub $ pure $ SumEditLeft $ MkWholeReaderEdit deleteValue
                        return ()
        let
            getWidgets :: f () -> View sel (OneWholeViews sel f)
            getWidgets fu =
                case retrieveOne fu of
                    FailureResult lfx -> do
                        vs <-
                            viewCreateView $ do
                                w <-
                                    makeEmptywidget $ \edits' ->
                                        liftIO $ run $ void $ pushEdit noEditSource $ subEdit asub edits'
                                lcContainPackStart True box w
                                widgetShow w
                        return $ MissingOVS lfx vs
                    SuccessResult () -> do
                        vs <-
                            viewCreateView $ do
                                widget <- baseView $ mapPureSubscriber (mustExistOneEditLens "object") sub
                                for_ mDeleteButton $ \button -> do
                                    lcContainPackStart False box button
                                    #show button
                                lcContainPackStart True box widget
                                widgetShow widget
                        return $ PresentOVS vs
        firstdvs <-
            cvLiftView $ do
                firstfu <- liftIO $ run $ subRead asub ReadHasOne
                getWidgets firstfu
        unliftView <- cvLiftView askUnliftIO
        cvDynamic sub firstdvs $ \_ -> do
            olddvs <- get
            newfu <- liftIO $ run $ subRead asub ReadHasOne
            case (olddvs, retrieveOne newfu) of
                (PresentOVS _, SuccessResult ()) -> return ()
                (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
                _ -> do
                    liftIO $ closeDynamicView olddvs
                    newdvs <- liftIO $ runWMFunction unliftView $ getWidgets newfu
                    put newdvs
        toWidget box

placeholderLabel :: CreateView sel Label
placeholderLabel = new Label [#label := "Placeholder"]

oneGetView :: GetGView
oneGetView =
    MkGetView $ \getview uispec -> do
        uit <- isUISpec uispec
        return $
            case uit of
                MaybeUISpec mnewval sub itemspec ->
                    oneWholeView (Just $ MkLimit Nothing) sub (createButton mnewval) $ \s -> getview $ itemspec s
                OneWholeUISpec sub itemspec ->
                    oneWholeView Nothing sub (\_ -> placeholderLabel) $ \s -> getview $ itemspec s
