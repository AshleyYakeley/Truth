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

catchActionResult :: IO a -> IO (Result SomeException a)
catchActionResult ioa = catch (fmap SuccessResult ioa) (return . FailureResult)

throwActionResult :: Result SomeException a -> IO a
throwActionResult (SuccessResult a) = return a
throwActionResult (FailureResult e) = throw e

testUIAction :: Bool -> Text -> (UIToolkit -> IO ()) -> ContextTestTree
testUIAction waitClick text testaction =
    contextTestCase text text $ \t -> do
        donevar <- newEmptyMVar
        truthMainGTK $ \MkTruthContext {..} -> do
            (pc, _) <- makeTestPinaforeContext AsynchronousUpdateTiming tcUIToolkit
            scriptaction <- let
                ?pinafore = pc
                in ioRunInterpretResult $ pinaforeInterpretFile "<test>" t
            liftIO scriptaction
            liftIO $
                case waitClick of
                    False -> do
                        ar <- catchActionResult $ testaction tcUIToolkit
                        putMVar donevar ar
                    True -> do
                        _ <-
                            forkIO $ do
                                let ui@MkUIToolkit {..} = tcUIToolkit
                                threadDelay 500000 -- .5s delay
                                ar <- uitWithLock $ catchActionResult $ testaction ui
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

gobjectEmitClicked ::
       forall t. GObject t
    => t
    -> IO ()
gobjectEmitClicked obj = do
    gtype <- gobjectType @t
    (_, signalId, detail) <- signalParseName "clicked" gtype False
    withManagedPtr obj $ \entryPtr -> do
        gvalObj <- buildGValue gtype set_object entryPtr
        _ <- signalEmitv [gvalObj] signalId detail
        return ()

testClickButton :: Bool -> Text -> ContextTestTree
testClickButton waitClick text =
    testUIAction waitClick text $ \MkUIToolkit {..} -> do
        ww <- windowListToplevels
        case ww of
            [w] -> do
                cc <- getAllWidgets w
                bb <- for cc $ castTo Button
                case catMaybes bb of
                    [b] -> gobjectEmitClicked b
                    _ -> fail "no single Button"
            _ -> fail "no single window"
        uitExit

testActions :: Bool -> [ContextTestTree]
testActions waitClick =
    [ testUIAction waitClick "return ()" $ \MkUIToolkit {..} -> uitExit
    , testUIAction waitClick "newpoint" $ \MkUIToolkit {..} -> uitExit
    , testUIAction waitClick "emptywindow" $ \MkUIToolkit {..} -> uitExit
    , testClickButton waitClick "buttonwindow $ return ()"
    , testClickButton waitClick "buttonwindow $ newMemFiniteSet"
    , testClickButton waitClick "buttonwindow $ newpoint"
    , testClickButton waitClick "buttonwindow $ emptywindow"
    , testClickButton waitClick "buttonwindow $ newpoint >> newpoint"
    , testClickButton waitClick "buttonwindow $ emptywindow >> emptywindow"
    , testClickButton waitClick "buttonwindow $ newpoint >> emptywindow"
    , testClickButton waitClick "buttonwindow $ emptywindow >> newpoint"
    ]

testUI :: TestTree
testUI =
    runContext $
    context
        [ "emptywindow :: Action ()"
        , "emptywindow = do openWindow {\"Empty\"} (\\_ -> {[]}) uiBlank ;return (); end"
        , "newpoint :: Action ()"
        , "newpoint = do s <- newMemFiniteSet; newEntity s; return (); end"
        , "buttonwindow :: Action Any -> Action ()"
        , "buttonwindow action = do openWindow {\"Test\"} (\\_ -> {[]}) (uiButton {\"Button\"} {action}); return (); end"
        ] $
    tgroup "UI" [tgroup "immediate" $ testActions False, tgroup "wait" $ testActions True]
