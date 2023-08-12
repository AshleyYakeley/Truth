module Lock
    ( lockTests
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Shapes
import Shapes.Test

lockTest :: String -> GView 'Unlocked a -> (a -> GView 'Unlocked ()) -> TestTree
lockTest name setup action =
    testTree name $ do
        task <-
            runLifecycle $
            runGTK $ \gtkc -> do
                runNewView $ do
                    a <- runGView gtkc setup
                    viewLiftLifecycle $
                        liftIOWithUnlift $ \unlift -> forkTask $ unlift $ runView $ runGView gtkc $ action a
        taskWait task

blankWindowSpec :: WindowSpec
blankWindowSpec = let
    wsPosition = WindowPositionCenter
    wsSize = (300, 400)
    wsCloseBoxAction = return ()
    wsTitle = constantModel "Test"
    wsContent _ = createBlank
    in MkWindowSpec {..}

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
              (gvRunLocked $
               gvRunUnlocked $
               gvRunLocked $ gvRunUnlocked $ gvRunLocked $ gvRunUnlocked $ gvRunLocked $ gvRunUnlocked $ return ())
              noAction
        , lockTest "onclose-locked" (gvOnClose @'Locked $ return ()) noAction
        , lockTest "onclose-unlocked" (gvOnClose @'Unlocked $ return ()) noAction
        , lockTest "lock-onclose-locked" (gvRunLocked $ gvOnClose @'Locked $ return ()) noAction
        , lockTest "lock-onclose-unlocked" (gvRunLocked $ gvOnClose @'Unlocked $ return ()) noAction
        , lockTest "lock-unlock-onclose-locked" (gvRunLocked $ gvRunUnlocked $ gvOnClose @'Locked $ return ()) noAction
        , lockTest
              "lock-unlock-onclose-unlocked"
              (gvRunLocked $ gvRunUnlocked $ gvOnClose @'Unlocked $ return ())
              noAction
        , lockTest
              "lock-unlock-onclose-locked-unlock"
              (gvRunLocked $ gvRunUnlocked $ gvOnClose @'Locked $ gvRunUnlocked $ return ())
              noAction
        , lockTest
              "lock-unlock-onclose-unlocked-lock"
              (gvRunLocked $ gvRunUnlocked $ gvOnClose @'Unlocked $ gvRunLocked $ return ())
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
                          gvLiftIONoUI $
                          forkIO $ cbRunLocked (gtkcLock gtkc) $ unlift $ gvRunUnlocked $ gvLiftIONoUI $ putMVar var ()
                  gvLiftIONoUI $ takeMVar var
              in lockTest "on-unlock" setup noAction
        , let
              setup :: GView 'Unlocked ()
              setup = do
                  var <- gvLiftIONoUI newEmptyMVar
                  gtkc <- gvGetContext
                  _tid <-
                      gvRunLocked $
                      gvWithUnliftLockedAsync $ \unlift ->
                          gvLiftIONoUI $
                          forkIO $ cbRunLocked (gtkcLock gtkc) $ unlift $ gvRunUnlocked $ gvLiftIONoUI $ putMVar var ()
                  gvLiftIONoUI $ takeMVar var
              in lockTest "locked-on-unlock" setup noAction
        , let
              setup :: GView 'Unlocked (GView 'Unlocked ())
              setup =
                  gvRunLocked $ do
                      (w, closer) <- gvGetCloser $ createWindow blankWindowSpec
                      uiWindowShow w
                      return $ gvRunLocked closer
              in lockTest "window" setup closeAction
        , let
              setup :: GView 'Unlocked (GView 'Unlocked ())
              setup =
                  gvRunLocked $ do
                      let
                          wspec =
                              blankWindowSpec
                                  {wsContent = \_ -> createDynamic $ constantModel $ gvRunLocked createBlank}
                      (w, closer) <- gvGetCloser $ createWindow wspec
                      uiWindowShow w
                      return $ gvRunLocked closer
              in lockTest "window-dynamic" setup closeAction
        ]
