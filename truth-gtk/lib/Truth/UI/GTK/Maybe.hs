module Truth.UI.GTK.Maybe
    ( oneGetView
    ) where

import GI.Gtk hiding (get)
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

data OneWholeViews sel f
    = MissingOVS (Limit f)
                 (ViewState sel)
    | PresentOVS (ViewState sel)

instance DynamicViewState (OneWholeViews sel f) where
    type DynamicViewSelEdit (OneWholeViews sel f) = sel
    dynamicViewStates (MissingOVS _ vs) = [vs]
    dynamicViewStates (PresentOVS vs) = [vs]
    dynamicViewFocus (MissingOVS _ vs) = Just vs
    dynamicViewFocus (PresentOVS vs) = Just vs

{-
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

oneWholeView ::
       forall sel f update wd. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update), IsWidget wd)
    => Maybe (Limit f)
    -> OpenSubscriber (FullResultOneUpdate f update)
    -> ((NonEmpty (FullResultOneEdit f (UpdateEdit update)) -> View sel ()) -> CreateView sel wd)
    -> (OpenSubscriber update -> GCreateView sel)
    -> GCreateView sel
oneWholeView mDeleteValue rmod@(MkOpenResource _ run asub) makeEmptywidget baseView = do
    box <- new Box [#orientation := OrientationVertical]
    mDeleteButton <-
        for mDeleteValue $ \(MkLimit deleteValue) -> do
            cvMakeButton "Delete" $
                liftIO $
                run $ do
                    _ <- pushEdit noEditSource $ subEdit asub $ pure $ SumEditLeft $ MkWholeReaderEdit deleteValue
                    return ()
    unliftView <- cvLiftView askUnliftIO
    let
        getWidgets :: OpenSubscriber (FullResultOneUpdate f update) -> f () -> View sel (OneWholeViews sel f)
        getWidgets rm fu =
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
                            widget <- baseView $ mapOpenSubscriber (mustExistOneEditLens "object") rm
                            for_ mDeleteButton $ \button -> do
                                lcContainPackStart False box button
                                #show button
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ PresentOVS vs
        initVS :: OpenSubscriber (FullResultOneUpdate f update) -> CreateView sel (OneWholeViews sel f)
        initVS rm = do
            firstfu <- liftIO $ withOpenResource rm $ \am -> subRead am ReadHasOne
            cvLiftView $ getWidgets rm firstfu
        recvVS :: [FullResultOneUpdate f update] -> StateT (OneWholeViews sel f) IO ()
        recvVS _ = do
            olddvs <- get
            newfu <- liftIO $ run $ subRead asub ReadHasOne
            case (olddvs, retrieveOne newfu) of
                (PresentOVS _, SuccessResult ()) -> return ()
                (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
                _ -> replaceDynamicView $ runWMFunction unliftView $ getWidgets rmod newfu
    cvDynamic rmod initVS recvVS
    toWidget box


placeholderLabel :: CreateView sel Label
placeholderLabel = new Label [#label := "Placeholder"]
-}
oneWholeView ::
       forall sel f update. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update))
    => OpenSubscriber (FullResultOneUpdate f update)
    -> (f (OpenSubscriber update) -> GCreateView sel)
    -> GCreateView sel
oneWholeView rmod@(MkOpenResource _ run asub) baseView = do
    box <- new Box [#orientation := OrientationVertical]
    {-
    mDeleteButton <-
        for mDeleteValue $ \(MkLimit deleteValue) -> do
            cvMakeButton "Delete" $
                liftIO $
                run $ do
                    _ <- pushEdit noEditSource $ subEdit asub $ pure $ SumEditLeft $ MkWholeReaderEdit deleteValue
                    return ()
    -}
    unliftView <- cvLiftView askUnliftIO
    let
        getWidgets :: OpenSubscriber (FullResultOneUpdate f update) -> f () -> View sel (OneWholeViews sel f)
        getWidgets rm fu =
            case retrieveOne fu of
                FailureResult lfx@(MkLimit fx) -> do
                    vs <-
                        viewCreateView $ do
                            widget <- baseView fx
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ MissingOVS lfx vs
                SuccessResult () -> do
                    vs <-
                        viewCreateView $ do
                            widget <- baseView $ pure $ mapOpenSubscriber (mustExistOneEditLens "object") rm
                            lcContainPackStart True box widget
                            widgetShow widget
                    return $ PresentOVS vs
        initVS :: OpenSubscriber (FullResultOneUpdate f update) -> CreateView sel (OneWholeViews sel f)
        initVS rm = do
            firstfu <- liftIO $ withOpenResource rm $ \am -> subRead am ReadHasOne
            cvLiftView $ getWidgets rm firstfu
        recvVS :: [FullResultOneUpdate f update] -> StateT (OneWholeViews sel f) IO ()
        recvVS _ = do
            olddvs <- get
            newfu <- liftIO $ run $ subRead asub ReadHasOne
            case (olddvs, retrieveOne newfu) of
                (PresentOVS _, SuccessResult ()) -> return ()
                (MissingOVS _ vs, FailureResult newlf) -> put $ MissingOVS newlf vs
                _ -> replaceDynamicView $ runWMFunction unliftView $ getWidgets rmod newfu
    cvDynamic rmod initVS mempty recvVS
    toWidget box

oneGetView :: GetGView
oneGetView =
    MkGetView $ \getview uispec -> do
        OneWholeUISpec sub itemspec <- isUISpec uispec
        return $ oneWholeView sub $ \fs -> getview $ itemspec fs
