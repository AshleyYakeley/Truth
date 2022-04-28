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
import Changes.Debug

data Timing
    = SyncTiming
    | AsyncTiming

instance Show Timing where
    show SyncTiming = "sync"
    show AsyncTiming = "async"

runUIAction :: forall a. Timing -> (ChangesContext -> View a) -> Text -> IO a
runUIAction timing testaction t = do
    donevar <- newEmptyMVar
    changesMainGTK $ \tc -> do
        (pc, _) <- viewLiftLifeCycle $ makeTestPinaforeContext tc stdout
        scriptaction <-
            runWithContext pc (libraryFetchModule gtkLibrary) $ throwInterpretResult $ pinaforeInterpretText "<test>" t
        scriptaction
        let
            testView :: View (Result SomeException a)
            testView = do
                ar <- tryExc $ testaction tc
                viewExitUI
                return ar
        case timing of
            SyncTiming -> do
                ar <- testView
                liftIO $ putMVar donevar ar
            AsyncTiming -> do
                _ <-
                    liftIO $
                    forkIO $ do
                        ar <- ccRunView tc emptyResourceContext testView
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
        _ <- traceBracket "signalEmitv" $ signalEmitv [gvalObj] signalId detail
        return ()

runClickButton :: ChangesContext -> View ()
runClickButton _ = do
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

noTestAction :: ChangesContext -> View ()
noTestAction _ = return ()

testUIAction :: Timing -> Text -> (ChangesContext -> View ()) -> ScriptTestTree
testUIAction timing text testaction = scriptTestCase text text $ runUIAction timing testaction

testActions :: Timing -> [ScriptTestTree]
testActions timing =
    [ testUIAction timing "return ()" noTestAction
    , testUIAction timing "newpoint" noTestAction
    , testUIAction timing "emptywindow" noTestAction
    , testUIAction timing "buttonwindow $ return ()" runClickButton
    , testUIAction timing "buttonwindow $ newMemFiniteSet" runClickButton
    , testUIAction timing "buttonwindow $ newpoint" runClickButton
    , testUIAction timing "buttonwindow $ emptywindow" runClickButton
    , testUIAction timing "buttonwindow $ newpoint >> newpoint" runClickButton
    , testUIAction timing "buttonwindow $ emptywindow >> emptywindow" runClickButton
    , testUIAction timing "buttonwindow $ newpoint >> emptywindow" runClickButton
    , testUIAction timing "buttonwindow $ emptywindow >> newpoint" runClickButton
    ]

testUI :: TestTree
testUI =
    runScriptTestTree $
    tDecls
        [ "emptywindow: Action Unit"
        , "emptywindow = do UI.openWindow (300,400) {\"Empty\"} {[]} UI.blank; return (); end"
        , "opentype T"
        , "newpoint: Action Unit"
        , "newpoint = do s <- newMemFiniteSet; p <- newOpenEntity @T; s += p; return (); end"
        , "buttonwindow: Action Any -> Action Unit"
        , "buttonwindow action = do UI.openWindow (300,400) {\"Test\"} {[]} (UI.button {\"Button\"} {action}); return (); end"
        ] $
    tGroup "UI" [tGroup "immediate" $ testActions SyncTiming, tGroup "wait" $ testActions AsyncTiming]
