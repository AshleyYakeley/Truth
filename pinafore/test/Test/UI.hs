module Test.UI
    ( testUI
    ) where

import Control.Exception
import Data.GI.Base.GValue
import GI.GObject
import GI.Gtk
import Pinafore
import Pinafore.Test
import Shapes
import Test.Context
import Test.Tasty
import Truth.Core
import Truth.UI.GTK
import Truth.Debug

catchActionResult :: IO a -> IO (Result SomeException a)
catchActionResult ioa = catch (fmap SuccessResult ioa) (return . FailureResult)

throwActionResult :: Result SomeException a -> IO a
throwActionResult (SuccessResult a) = return a
throwActionResult (FailureResult e) = throw e

testUIAction :: Text -> (UIToolkit -> IO ()) -> ContextTestTree
testUIAction text testaction =
    contextTestCase text text $ \t -> traceBracket ("TEST: " <> unpack text) $ do
        donevar <- newEmptyMVar
        truthMainGTK True $ \MkTruthContext {..} -> do
            (pc, _) <- makeTestPinaforeContext True tcUIToolkit
            scriptaction <- let
                ?pinafore = pc
                in pinaforeInterpretFile "<test>" t
            liftIO scriptaction
            _ <-
                liftIO $
                forkIO $ do
                    let ui@MkUIToolkit {..} = tcUIToolkit
                    ar <- uitCallFromOtherThread $ catchActionResult $ testaction ui
                    putMVar donevar ar
            return ()
        ar <- takeMVar donevar
        throwActionResult ar

getAllWidgets :: Widget -> IO [Widget]
getAllWidgets w = do
    mcont <- castTo Container w
    case mcont of
        Nothing -> return [w]
        Just cont -> do
            cc <- #getChildren cont
            ww <- for cc getAllWidgets
            return $ w : mconcat ww

gobjectEmitClicked :: GObject t => t -> IO ()
gobjectEmitClicked obj = do
    gtype <- gobjectType obj
    (_, signalId, detail) <- signalParseName "clicked" gtype False
    withManagedPtr obj $ \entryPtr -> do
        gvalObj <- buildGValue gtype set_object entryPtr
        _ <- signalEmitv [gvalObj] signalId detail
        return ()

testClickButton :: Text -> ContextTestTree
testClickButton text =
    testUIAction text $ \MkUIToolkit {..} -> do
        ww <- windowListToplevels
        case ww of
            [w] -> do
                cc <- getAllWidgets w
                bb <- for cc $ castTo Button
                case catMaybes bb of
                    [b] -> gobjectEmitClicked b
                    _ -> fail "no single Button"
            _ -> fail "no single window"
        uitCloseAllWindows

testUI :: TestTree
testUI =
    runContext $
    context
        [ "emptywindow :: Action ()"
        , "emptywindow = do openwindow {\"Empty\"} (\\_ -> {[]}) ui_blank ;return (); end"
        , "newpoint :: Action ()"
        , "newpoint = do s <- newmemset; newentity s; return (); end"
        , "buttonwindow :: Action () -> Action ()"
        , "buttonwindow action = do openwindow {\"Test\"} (\\_ -> {[]}) (ui_button {\"Button\"} {action}); return (); end"
        ] $
    tgroup
        "UI"
        [ testUIAction "return ()" $ \MkUIToolkit {..} -> uitCloseAllWindows
        , testUIAction "newpoint" $ \MkUIToolkit {..} -> uitCloseAllWindows
        , testUIAction "emptywindow" $ \MkUIToolkit {..} -> uitCloseAllWindows
        , testClickButton "buttonwindow $ return ()"
        , testClickButton "buttonwindow newpoint"
        , testClickButton "buttonwindow $ emptywindow"
        , testClickButton "buttonwindow $ newpoint >> newpoint"
        , testClickButton "buttonwindow $ emptywindow >> emptywindow"
        , testClickButton "buttonwindow $ newpoint >> emptywindow"
        , testClickButton "buttonwindow $ emptywindow >> newpoint"
        ]
