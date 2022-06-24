module Main
    ( main
    ) where

import Changes.Core
import Changes.UI.GTK
import qualified GI.Cairo.Render.Matrix as RM
import Shapes
import Shapes.Test

lockTest :: String -> GView 'Unlocked a -> (a -> GView 'Unlocked ()) -> TestTree
lockTest name setup action =
    testTree name $ do
        task <-
            runLifeCycleT $
            runGTK $ \gtkc -> do
                runNewView $ do
                    a <- runGView gtkc setup
                    viewLiftLifeCycle $
                        liftIOWithUnlift $ \unlift -> forkSingleTask $ runView unlift $ runGView gtkc $ action a
        taskWait task

blankWindowSpec :: WindowSpec
blankWindowSpec = let
    wsPosition = WindowPositionCenter
    wsSize = (300, 400)
    wsCloseBoxAction = return ()
    wsTitle = constantModel "Test"
    wsMenuBar = Nothing
    wsContent = createBlank
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
        , let
              setup :: GView 'Unlocked (GView 'Unlocked ())
              setup =
                  gvRunLocked $ do
                      (w, closer) <- gvGetCloser $ createWindow blankWindowSpec
                      uiWindowShow w
                      return closer
              in lockTest "window" setup closeAction
        , let
              setup :: GView 'Unlocked (GView 'Unlocked ())
              setup =
                  gvRunLocked $ do
                      let wspec = blankWindowSpec {wsContent = createDynamic $ constantModel createBlank}
                      (w, closer) <- gvGetCloser $ createWindow wspec
                      uiWindowShow w
                      return closer
              in lockTest "window-dynamic" setup closeAction
        ]

matrixTest :: TestTree
matrixTest =
    testTree "matrix" $ let
        m = RM.translate 1 2 $ RM.scale 4 (0.5) $ RM.translate 2 1 $ RM.identity
        p0 = (3, 5)
        p1 = RM.transformPoint m p0
        p2 = RM.transformPoint (RM.invert m) p1
        in assertEqual "" p0 p2

tests :: TestTree
tests = testTree "changes-gtk" [matrixTest, lockTests]

main :: IO ()
main = testMain tests
