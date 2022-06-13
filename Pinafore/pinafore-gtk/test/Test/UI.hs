module Test.UI
    ( testUI
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.GI.Base.GValue
import GI.GObject
import GI.Gtk
import Pinafore
import Pinafore.Language.Library.GTK
import Pinafore.Test
import Shapes hiding (get)
import Shapes.Test
import Test.Context

data Timing
    = SyncTiming
    | AsyncTiming

instance Show Timing where
    show SyncTiming = "sync"
    show AsyncTiming = "async"

runUIAction :: forall a. Timing -> View a -> Text -> IO a
runUIAction timing testaction t = do
    donevar <- newEmptyMVar
    runLifeCycleT $
        runNewView $ do
            (pc, _) <- viewLiftLifeCycle $ makeTestPinaforeContext stdout
            Known gtkc <-
                runWithContext pc (libraryFetchModule gtkLibrary) $ do
                    scriptaction <-
                        throwInterpretResult $ pinaforeInterpretTextAtType @(PinaforeAction GTKContext) "<test>" t
                    unliftPinaforeAction scriptaction
            let
                testView :: View (Result SomeException a)
                testView = do
                    ar <- tryExc $ testaction
                    runGView gtkc gvExitUI
                    return ar
            case timing of
                SyncTiming -> do
                    ar <- testView
                    liftIO $ putMVar donevar ar
                AsyncTiming -> do
                    _ <-
                        viewLiftLifeCycle $
                        liftIOWithUnlift $ \unlift ->
                            forkIO $ do
                                ar <- runView unlift testView
                                putMVar donevar ar
                    return ()
    ar <- takeMVar donevar
    throwResult ar

getAllWidgets :: Widget -> View [Widget]
getAllWidgets w = do
    mcont <- liftIO $ castTo Container w
    case mcont of
        Nothing -> return [w]
        Just cont -> do
            cc <- #getChildren cont
            ww <- for cc getAllWidgets
            return $ w : mconcat ww

gobjectEmitClicked ::
       forall t. GObject t
    => t
    -> IO ()
gobjectEmitClicked obj = do
    gtype <- glibType @t
    (_, signalId, detail) <- signalParseName "clicked" gtype False
    withManagedPtr obj $ \entryPtr -> do
        gvalObj <- buildGValue gtype set_object entryPtr
        _ <- signalEmitv [gvalObj] signalId detail
        return ()

runClickButton :: View ()
runClickButton = do
    ww <- windowListToplevels
    visww <-
        for ww $ \w -> do
            v <- get w #visible
            return $
                if v
                    then Just w
                    else Nothing
    case catMaybes visww of
        [w] -> do
            cc <- getAllWidgets w
            bb <- liftIO $ for cc $ castTo Button
            case catMaybes bb of
                [b] -> liftIO $ gobjectEmitClicked b
                _ -> fail $ show (length bb) <> " Buttons"
        _ -> fail $ show (length ww) <> " visible windows"

noTestAction :: View ()
noTestAction = return ()

testUIAction :: Timing -> Text -> View () -> ScriptTestTree
testUIAction timing text testaction =
    scriptTestCase text ("do gtk <- UI.run; " <> text <> "; return gtk; end") $ runUIAction timing testaction

testActions :: Timing -> [ScriptTestTree]
testActions timing =
    [ testUIAction timing "return ()" noTestAction
    , testUIAction timing "newpoint" noTestAction
    , testUIAction timing "emptywindow gtk" noTestAction
    , testUIAction timing "buttonwindow gtk $ return ()" runClickButton
    , testUIAction timing "buttonwindow gtk $ newMemFiniteSet" runClickButton
    , testUIAction timing "buttonwindow gtk $ newpoint" runClickButton
    , testUIAction timing "buttonwindow gtk $ emptywindow" runClickButton
    , testUIAction timing "buttonwindow gtk $ newpoint >> newpoint" runClickButton
    , testUIAction timing "buttonwindow gtk $ emptywindow >> emptywindow" runClickButton
    , testUIAction timing "buttonwindow gtk $ newpoint >> emptywindow" runClickButton
    , testUIAction timing "buttonwindow gtk $ emptywindow >> newpoint" runClickButton
    ]

testUI :: TestTree
testUI =
    runScriptTestTree $
    tDecls
        [ "emptywindow: UI.Context -> Action Unit"
        , "emptywindow gtk = do UI.openWindow gtk (300,400) {\"Empty\"} {[]} UI.blank; return (); end"
        , "opentype T"
        , "newpoint: Action Unit"
        , "newpoint = do s <- newMemFiniteSet; p <- newOpenEntity @T; s += p; return (); end"
        , "buttonwindow: UI.Context -> Action Any -> Action Unit"
        , "buttonwindow gtk action = do UI.openWindow gtk (300,400) {\"Test\"} {[]} (UI.button {\"Button\"} {action}); return (); end"
        ] $
    tGroup "UI" [tGroup "immediate" $ testActions SyncTiming, tGroup "wait" $ testActions AsyncTiming]
