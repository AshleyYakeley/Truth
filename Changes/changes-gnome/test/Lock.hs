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
    testTree name $ runLifecycle $ do
        (task, _) <-
            do
                gtkc <- runGTK
                runView $ do
                    a <- runGView gtkc setup
                    viewLiftLifecycle
                        $ liftIOWithUnlift
                        $ \unlift -> forkTask $ unlift $ runView $ runGView gtkc $ action a
        liftIO $ taskWait task

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

lockTests :: TestTree
lockTests =
    testTree
        "lock"
        [ testTree @(IO ()) "run" $ runLifecycle $ do
            _ <- runGTK
            return ()
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
                                    $ \_ ->
                                        unlift
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
            in lockTest "window" setup closeAction
        , let
            setup :: GView 'Unlocked (GSemiview 'Unlocked ())
            setup =
                gvRunLocked $ do
                    let wspec = blankWindowSpec{wsContent = createDynamic $ constantModel $ createBlank}
                    (w, closer) <- gvRunUnlocked $ gvGetCloser $ createWindow wspec
                    #present w
                    return closer
            in lockTest "window-dynamic" setup closeAction
        ]
