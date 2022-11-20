module Test.UI
    ( testUI
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Changes.World.GNOME.GTK.Test
import Pinafore
import Pinafore.Language.Library.GNOME
import Pinafore.Test
import Shapes hiding (get)
import Shapes.Test
import Test.Context

runUIAction :: GView 'Unlocked () -> Text -> IO ()
runUIAction testaction script =
    runTester defaultTester {tstFetchModule = libraryFetchModule gnomeLibrary} $ do
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
    scriptTestCase text ("fn call => GTK.run $ fn gtk => do " <> text <> "; call gtk; end") $ runUIAction testaction

testActions :: ScriptTestTree
testActions =
    tGroup
        "GTK"
        [ testUIAction "return ()" noTestAction
        , testUIAction "newpoint" noTestAction
        , testUIAction "emptywindow gtk" noTestAction
        , testUIAction "buttonwindow gtk $ return ()" noTestAction
        , testUIAction "buttonwindow gtk $ return ()" runClickButton
        , testUIAction "buttonwindow gtk $ newMemFiniteSetModel" runClickButton
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
        [ "import \"pinafore-gnome\""
        , "emptywindow: GTK.Context -> Action Unit = fn gtk => do GTK.openWindow gtk (300,400) {\"Empty\"} GTK.blank; return (); end"
        , "opentype T"
        , "newpoint: Action Unit = do s <- newMemFiniteSetModel; p <- newOpenEntity @T; s += p; return (); end"
        , "buttonwindow: GTK.Context -> Action Any -> Action Unit = fns gtk action => do GTK.openWindow gtk (300,400) {\"Test\"} (GTK.button {\"Button\"} {action}); return (); end"
        ]
        testActions
