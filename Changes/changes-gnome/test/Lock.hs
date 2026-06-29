module Lock
    ( lockTests
    )
where

import Changes.Core
import Shapes
import Shapes.Test

import Changes.World.GNOME.GTK
import Changes.World.GNOME.GTK.Test

lockTest :: String -> GView 'Unlocked a -> (a -> GView 'Unlocked ()) -> TestTree
lockTest name setup action =
    testTree name $ do
        (task, _) <-
            runLifecycle
                $ runGTK
                $ \gtkc -> do
                    runView $ do
                        a <- runGView gtkc setup
                        viewLiftLifecycle
                            $ liftIOWithUnlift
                            $ \unlift -> forkTask $ unlift $ runView $ runGView gtkc $ action a
        taskWait task

blankWindowSpec :: WindowSpec
blankWindowSpec = let
    wsSize = (300, 400)
    wsCloseBoxAction = return ()
    wsTitle = constantModel "Test"
    wsContent = createBlank
    in MkWindowSpec{..}

noAction :: a -> GView 'Unlocked ()
noAction _ = return ()

closeAction :: GSemiview 'Unlocked () -> GView 'Unlocked ()
closeAction closer = lift $ do
    gsvSleep 50000
    closer

checkSingleChildType :: Text -> Widget -> GView 'Locked ()
checkSingleChildType expected widget = do
    children <- gvLiftIO $ getWidgetChildren widget
    childTypes <- for children getObjectTypeName
    gvLiftIO $ assertEqual "child type" [expected] childTypes

checkSingleChildName :: Text -> Widget -> GView 'Locked ()
checkSingleChildName expected widget = do
    children <- gvLiftIO $ getWidgetChildren widget
    childNames <- for children $ \child -> #getName child
    gvLiftIO $ assertEqual "child name" [expected] childNames

createNamedTextView :: Text -> Text -> GView 'Unlocked Widget
createNamedTextView name text = do
    model <- gvLiftLifecycle $ makeMemoryModel text
    widget <- createTextView (mapModel convertChangeLens model) mempty
    gvRunLocked $ setCSSName name widget
    return widget

readWholeModel :: Model (WholeUpdate a) -> GView 'Unlocked a
readWholeModel model = gvLiftView $ viewRunResource model $ \amodel -> aModelRead amodel ReadWhole

rejectingWholeTextModel :: Text -> Model (WholeUpdate Text)
rejectingWholeTextModel text = mapModel fromReadOnlyRejectingChangeLens $ constantModel text

rejectingStringTextModel :: Text -> Model (StringUpdate Text)
rejectingStringTextModel text = mapModel convertChangeLens $ rejectingWholeTextModel text

lockTests :: TestTree
lockTests =
    inOrderTestGroup
        "lock"
        [ testTree @(IO ()) "run" $ runLifecycle $ runGTK $ \_ -> return ()
        , lockTest "return" (return ()) noAction
        , lockTest "lock" (gvRunLocked $ return ()) noAction
        , lockTest "lock-unlock-1" (gvRunLocked $ gvRunUnlocked $ return ()) noAction
        , lockTest
            "lock-unlock-2"
            ( gvRunLocked
                $ gvRunUnlocked
                $ gvRunLocked
                $ gvRunUnlocked
                $ return ()
            )
            noAction
        , lockTest
            "lock-unlock-4"
            ( gvRunLocked
                $ gvRunUnlocked
                $ gvRunLocked
                $ gvRunUnlocked
                $ gvRunLocked
                $ gvRunUnlocked
                $ gvRunLocked
                $ gvRunUnlocked
                $ return ()
            )
            noAction
        , lockTest "onclose-unlocked" (gvOnClose @'Unlocked $ return ()) noAction
        , lockTest "lock-onclose-locked" (gvRunLocked $ gvOnClose @'Locked $ return ()) noAction
        , lockTest "lock-unlock-onclose-locked" (gvRunLocked $ gvRunUnlocked $ gvOnClose @'Locked $ return ()) noAction
        , lockTest "lock-unlock-onclose-unlocked" (gvRunLocked $ gvRunUnlocked $ gvOnClose @'Unlocked $ return ()) noAction
        , lockTest
            "lock-unlock-onclose-unlocked-lock"
            (gvRunLocked $ gvRunUnlocked $ gvOnClose $ gsvRunLocked $ return ())
            noAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                var <- gvLiftIOTrustMeNoUI newEmptyMVar
                gtkc <- gvGetContext
                _tid <-
                    gvRunLocked
                        $ gvRunUnlockedAllowAsync
                        $ gvWithAsyncUnlift ()
                        $ \unlift -> gvLiftIOTrustMeNoUI
                            $ forkIO
                            $ do
                                threadSleep 0.1
                                runLockedIO gtkc
                                    $ unlift
                                    $ gvLiftIOTrustMeNoUI
                                    $ putMVar var ()
                gvLiftIOTrustMeNoUI $ takeMVar var
            in lockTest "on" setup noAction
        , let
            setup :: GView 'Unlocked (GSemiview 'Unlocked ())
            setup =
                gvRunLocked $ do
                    (w, closer) <- gvRunUnlocked $ gvGetCloser $ createWindow blankWindowSpec
                    #present w
                    return closer
            in lockTest "window-simple" setup closeAction
        , let
            setup :: GView 'Unlocked (GSemiview 'Unlocked ())
            setup =
                gvRunLocked $ do
                    let wspec = blankWindowSpec{wsContent = createDynamic $ constantModel $ createBlank}
                    (w, closer) <- gvRunUnlocked $ gvGetCloser $ createWindow wspec
                    #present w
                    return closer
            in lockTest "window-dynamic" setup closeAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                model <- gvLiftLifecycle $ makeMemoryModel False
                widget <-
                    createDynamic
                        $ mapModel
                            ( funcChangeLens
                                $ \showLabel ->
                                    if showLabel
                                        then createLabel $ constantModel "shown"
                                        else createBlank
                            )
                        $ modelToReadOnly model
                gvRunLocked $ checkSingleChildType "GtkDrawingArea" widget
                _ <- gvSetWholeModel model noEditSource True
                gvLiftView $ viewWaitUpdates model
                gvRunLocked $ checkSingleChildType "GtkLabel" widget
                _ <- gvSetWholeModel model noEditSource False
                gvLiftView $ viewWaitUpdates model
                gvRunLocked $ checkSingleChildType "GtkDrawingArea" widget
            in lockTest "widget-dynamic-switch" setup noAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                model <- gvLiftLifecycle $ makeMemoryModel False
                widget <-
                    createDynamic
                        $ mapModel
                            ( funcChangeLens
                                $ \showFirst ->
                                    if showFirst
                                        then createNamedTextView "first-text" "first"
                                        else createNamedTextView "second-text" "second"
                            )
                        $ modelToReadOnly model
                gvRunLocked $ checkSingleChildName "second-text" widget
                _ <- gvSetWholeModel model noEditSource True
                gvLiftView $ viewWaitUpdates model
                gvRunLocked $ checkSingleChildName "first-text" widget
                _ <- gvSetWholeModel model noEditSource False
                gvLiftView $ viewWaitUpdates model
                gvRunLocked $ checkSingleChildName "second-text" widget
            in lockTest "widget-dynamic-text-switch" setup noAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                model <- gvLiftLifecycle $ makeMemoryModel $ Just False
                widget <- createMaybeCheckButton (constantModel "check") model
                gvRunLocked $ do
                    _ <- #activate widget
                    return ()
                gvLiftView $ viewWaitUpdates model
                readWholeModel model >>= gvLiftIOTrustMeNoUI . assertEqual "checked" (Just True)
                gvRunLocked $ do
                    _ <- #activate widget
                    return ()
                gvLiftView $ viewWaitUpdates model
                readWholeModel model >>= gvLiftIOTrustMeNoUI . assertEqual "unchecked" (Just False)
            in lockTest "maybe-check-button-activate" setup noAction
        , lockTest "text-entry-reject-reset" testTextEntry noAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                buffer <- createTextBuffer (rejectingStringTextModel "constant") mempty
                gvRunLocked $ do
                    #setText buffer "changed" (-1)
                    start <- #getStartIter buffer
                    end <- #getEndIter buffer
                    text <- #getText buffer start end True
                    gvLiftIO $ assertEqual "buffer text" "constant" text
            in lockTest "text-buffer-reject-reset" setup noAction
        ]
