module Lock
    ( lockTests
    )
where

import Changes.Core
import Shapes
import Shapes.Test

import Changes.World.GNOME.GTK

lockTest :: String -> GView 'Unlocked a -> (a -> GView 'Unlocked ()) -> TestTree
lockTest name setup action =
    testTree name $ do
        task <-
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
    wsPosition = WindowPositionCenter
    wsSize = (300, 400)
    wsCloseBoxAction = return ()
    wsTitle = constantModel "Test"
    wsContent _ = createBlank
    in MkWindowSpec{..}

noAction :: a -> GView 'Unlocked ()
noAction _ = return ()

closeAction :: GView 'Unlocked () -> GView 'Unlocked ()
closeAction closer = do
    gvSleep 50000
    closer

lockTests :: TestTree
lockTests =
    testTree
        "lock"
        [ lockTest "return" (return ()) noAction
        , lockTest "lock" (gvRunLocked $ return ()) noAction
        , lockTest "lock-unlock" (gvRunLocked $ gvRunUnlocked $ return ()) noAction
        , lockTest
            "lock-unlock-n"
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
        , lockTest "onclose-unlocked" (gvOnClose $ return ()) noAction
        , lockTest "lock-onclose-locked" (gvRunLocked $ gvOnClose $ return ()) noAction
        , lockTest "lock-unlock-onclose-locked" (gvRunLocked $ gvRunUnlocked $ gvOnClose $ return ()) noAction
        , lockTest "lock-unlock-onclose-unlocked" (gvRunLocked $ gvRunUnlocked $ gvOnClose $ return ()) noAction
        , lockTest
            "lock-unlock-onclose-unlocked-lock"
            (gvRunLocked $ gvRunUnlocked $ gvOnClose $ gvRunLocked $ return ())
            noAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                var <- gvLiftIONoUI newEmptyMVar
                gtkc <- gvGetContext
                _tid <-
                    gvWithUnliftLockedAsync $ \unlift ->
                        gvLiftIONoUI $ forkIO $ cbRunLocked (gtkcLock gtkc) $ unlift $ gvLiftIONoUI $ putMVar var ()
                gvLiftIONoUI $ takeMVar var
            in lockTest "on" setup noAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                var <- gvLiftIONoUI newEmptyMVar
                gtkc <- gvGetContext
                _tid <-
                    gvWithUnliftLockedAsync $ \unlift ->
                        gvLiftIONoUI
                            $ forkIO
                            $ cbRunLocked (gtkcLock gtkc)
                            $ unlift
                            $ gvRunUnlocked
                            $ gvLiftIONoUI
                            $ putMVar var ()
                gvLiftIONoUI $ takeMVar var
            in lockTest "on-unlock" setup noAction
        , let
            setup :: GView 'Unlocked ()
            setup = do
                var <- gvLiftIONoUI newEmptyMVar
                gtkc <- gvGetContext
                _tid <-
                    gvRunLocked
                        $ gvWithUnliftLockedAsync
                        $ \unlift ->
                            gvLiftIONoUI
                                $ forkIO
                                $ cbRunLocked (gtkcLock gtkc)
                                $ unlift
                                $ gvRunUnlocked
                                $ gvLiftIONoUI
                                $ putMVar var ()
                gvLiftIONoUI $ takeMVar var
            in lockTest "locked-on-unlock" setup noAction
        , let
            setup :: GView 'Unlocked (GView 'Unlocked ())
            setup =
                gvRunLocked $ do
                    (w, closer) <- gvRunUnlocked $ gvGetCloser $ createWindow blankWindowSpec
                    uiWindowShow w
                    return closer
            in lockTest "window" setup closeAction
        , let
            setup :: GView 'Unlocked (GView 'Unlocked ())
            setup =
                gvRunLocked $ do
                    let wspec = blankWindowSpec{wsContent = \_ -> createDynamic $ constantModel $ createBlank}
                    (w, closer) <- gvRunUnlocked $ gvGetCloser $ createWindow wspec
                    uiWindowShow w
                    return closer
            in lockTest "window-dynamic" setup closeAction
        ]
