module Test.UI
    ( testUI
    ) where

import Changes.Core
import Changes.Debug
import Changes.UI.GTK
import Changes.UI.GTK.Test
import Pinafore
import Pinafore.Language.Library.GTK
import Pinafore.Test
import Shapes hiding (get)
import Shapes.Test
import Test.Context

runUIAction :: GView 'Unlocked () -> Text -> IO ()
runUIAction testaction script =
    runLifeCycleT $
    runNewView $ do
        (pc, _) <- viewLiftLifeCycle $ makeTestPinaforeContext stdout
        runWithContext pc (libraryFetchModule gtkLibrary) $ do
            scriptaction <-
                throwInterpretResult $
                pinaforeInterpretTextAtType @((GTKContext -> View ()) -> PinaforeAction ()) "<test>" script
            donevar <- liftIO newEmptyMVar
            runPinaforeAction $
                scriptaction $ \gtkc -> do
                    _ <-
                        liftIOWithUnlift $ \unlift ->
                            traceForkIO "action" $
                            unlift $
                            runGView gtkc $ do
                                traceBracket "sleep" $ gvSleep 50000
                                traceBracket "action" testaction
                                traceBracket "exit" gvExitUI
                                liftIO $ traceBracket "done" $ putMVar donevar ()
                    return ()
            liftIO $ traceBracket "wait for done" $ takeMVar donevar

runClickButton :: GView 'Unlocked ()
runClickButton = traceBracket "runClickButton" $ gvRunLocked clickOnlyWindowButton

noTestAction :: GView ls ()
noTestAction = return ()

testUIAction :: Text -> GView 'Unlocked () -> ScriptTestTree
testUIAction text testaction =
    scriptTestCase text ("\\call => GTK.run $ \\gtk => do " <> text <> "; call gtk; end") $ runUIAction testaction

testActions :: ScriptTestTree
testActions =
    tGroup
        "GTK"
        [ testUIAction "return ()" noTestAction
        , testUIAction "newpoint" noTestAction
        , testUIAction "emptywindow gtk" noTestAction
        , testUIAction "buttonwindow gtk $ return ()" noTestAction
        , testUIAction "buttonwindow gtk $ return ()" runClickButton
        , testUIAction "buttonwindow gtk $ newMemFiniteSet" runClickButton
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
        [ "emptywindow: GTK.Context -> Action Unit"
        , "emptywindow gtk = do GTK.openWindow gtk (300,400) {\"Empty\"} {[]} GTK.blank; return (); end"
        , "opentype T"
        , "newpoint: Action Unit"
        , "newpoint = do s <- newMemFiniteSet; p <- newOpenEntity @T; s += p; return (); end"
        , "buttonwindow: GTK.Context -> Action Any -> Action Unit"
        , "buttonwindow gtk action = do GTK.openWindow gtk (300,400) {\"Test\"} {[]} (GTK.button {\"Button\"} {action}); return (); end"
        ]
        testActions
