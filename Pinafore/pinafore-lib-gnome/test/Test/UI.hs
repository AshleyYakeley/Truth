module Test.UI
    ( testUI
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Changes.World.GNOME.GTK.Test
import Pinafore.API
import Pinafore.Library.GNOME
import Pinafore.Test
import Shapes hiding (get)
import Shapes.Test
import Test.Context

runUIAction :: GView 'Unlocked () -> Text -> IO ()
runUIAction testaction script =
    runTester defaultTester $
    testerLoadLibrary gnomeLibrary $ do
        scriptaction <- testerLiftView $ qInterpretTextAtType @((LangContext -> View ()) -> Action ()) "<test>" script
        donevar <- liftIO newEmptyMVar
        testerLiftAction $
            scriptaction $ \lc -> do
                _ <-
                    liftIOWithUnlift $ \unlift ->
                        forkIO $
                        unlift $
                        runGView (lcGTKContext lc) $ do
                            gvSleep 50000
                            testaction
                            gvExitUI
                            gvLiftIONoUI $ putMVar donevar ()
                return ()
        liftIO $ takeMVar donevar

runClickButton :: GView 'Unlocked ()
runClickButton = gvRunLocked clickOnlyWindowButton

noTestAction :: GView ls ()
noTestAction = return ()

testUIAction :: Text -> GView 'Unlocked () -> ScriptTestTree
testUIAction text testaction =
    scriptTestCase text ("fn call => run.GTK $ fn gtk => do " <> text <> "; call gtk; end") $ runUIAction testaction

testActions :: ScriptTestTree
testActions =
    tGroup
        "GTK"
        [ testUIAction "pure ()" noTestAction
        , testUIAction "newpoint" noTestAction
        , testUIAction "emptywindow gtk" noTestAction
        , testUIAction "buttonwindow gtk $ pure ()" noTestAction
        , testUIAction "buttonwindow gtk $ pure ()" runClickButton
        , testUIAction "buttonwindow gtk $ newMem.FiniteSetModel" runClickButton
        , testUIAction "buttonwindow gtk $ newpoint" runClickButton
        , testUIAction "buttonwindow gtk $ emptywindow gtk" runClickButton
        , testUIAction "buttonwindow gtk $ newpoint >> newpoint" runClickButton
        , testUIAction "buttonwindow gtk $ emptywindow gtk >> emptywindow gtk" runClickButton
        , testUIAction "buttonwindow gtk $ newpoint >> emptywindow gtk" runClickButton
        , testUIAction "buttonwindow gtk $ emptywindow gtk >> newpoint" runClickButton
        ]

testUI :: TestTree
testUI =
    runScriptTestTree $
    tDecls
        [ "with Function, SetModel, Action end"
        , "import \"gnome\" end"
        , "emptywindow: Context.GTK -> Action Unit = fn gtk => do open.Window.GTK gtk (300,400) {\"Empty\"} blank.Widget.GTK; pure (); end"
        , "entitytype T"
        , "newpoint: Action Unit = do s <- newMem.FiniteSetModel; p <- new.OpenEntity @T; s += p; pure (); end"
        , "buttonwindow: Context.GTK -> Action Any -> Action Unit = fn gtk, action => do open.Window.GTK gtk (300,400) {\"Test\"} (button.Widget.GTK {\"Button\"} {action}); pure (); end"
        ]
        testActions
