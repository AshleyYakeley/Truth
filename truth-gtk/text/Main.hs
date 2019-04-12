module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Shapes
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.UI.GTK
import Truth.World.Charset
import Truth.World.File

textCodec :: ReasonCodec LazyByteString Text
textCodec = bijectionCodec packBijection . utf8Codec . bijectionCodec unpackBijection

textLens :: EditLens ByteStringEdit (WholeEdit ((Result Text) Text))
textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) . convertEditLens

optParser :: O.Parser ([FilePath], Bool, Bool)
optParser = (,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "save")

async :: Bool
async = False

main :: IO ()
main =
    truthMainGTK async $ \MkTruthContext {..} ->
        liftIO $ do
            (paths, double, saveOpt) <-
                O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) tcArguments
            for_ paths $ \path -> do
                let
                    bsObj :: Object ByteStringEdit
                    bsObj = fileObject path
                    wholeTextObj :: Object (WholeEdit ((Result Text) Text))
                    wholeTextObj = cacheWholeObject $ mapObject textLens bsObj
                    ui :: Subscriber (OneWholeEdit (Result Text) (StringEdit Text))
                       -> (forall sel edit. UIWindow -> UISpec sel edit -> (MenuBar edit, UISpec sel edit))
                       -> UISpec (EditLens (StringEdit Text) (StringEdit Text)) (OneWholeEdit (Result Text) (StringEdit Text))
                    ui sub extraui =
                        withAspectUISpec $ \aspect -> let
                            openSelection :: IO ()
                            openSelection = do
                                mlens <- aspect
                                case mlens of
                                    Nothing -> return ()
                                    Just lens ->
                                        makeWindow "section" (mapSubscriber (oneWholeLiftEditLens lens) sub) extraui
                            in verticalUISpec
                                   [ (simpleButtonUISpec (constEditFunction "View") openSelection, False)
                                   , (scrolledUISpec $ oneWholeUISpec textAreaUISpec, True)
                                   ]
                    MkUIToolkit {..} = tcUIToolkit
                    makeWindow ::
                           Text
                        -> Subscriber (OneWholeEdit (Result Text) (StringEdit Text))
                        -> (forall sel edit. UIWindow -> UISpec sel edit -> (MenuBar edit, UISpec sel edit))
                        -> IO ()
                    makeWindow title sub extraui = do
                        rec
                            let (mbar, uic) = extraui r $ ui sub extraui
                            r <-
                                uitCreateWindow sub $
                                MkWindowSpec (constEditFunction title) (Just $ \_ -> constEditFunction mbar) uic
                        return ()
                    simpleUI :: forall sel edit. UIWindow -> UISpec sel edit -> (MenuBar edit, UISpec sel edit)
                    simpleUI ~MkUIWindow {..} spec = let
                        mbar :: MenuBar edit
                        mbar =
                            [ SubMenuEntry
                                  "File"
                                  [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') uiWindowClose
                                  , SeparatorMenuEntry
                                  , simpleActionMenuItem
                                        "Exit"
                                        (Just $ MkMenuAccelerator [KMCtrl] 'Q')
                                        uitCloseAllWindows
                                  ]
                            ]
                        in (mbar, spec)
                    extraUI ::
                           forall sel edit.
                           SaveActions
                        -> UndoActions
                        -> UIWindow
                        -> UISpec sel edit
                        -> (MenuBar edit, UISpec sel edit)
                    extraUI (MkSaveActions saveActions) (MkUndoActions undo redo) ~MkUIWindow {..} spec = let
                        saveAction = do
                            mactions <- saveActions
                            _ <-
                                case mactions of
                                    Just (action, _) -> action noEditSource
                                    _ -> return False
                            return ()
                        revertAction = do
                            mactions <- saveActions
                            _ <-
                                case mactions of
                                    Just (_, action) -> action noEditSource
                                    _ -> return False
                            return ()
                        mbar :: [MenuEntry edit]
                        mbar =
                            [ SubMenuEntry
                                  "File"
                                  [ simpleActionMenuItem "Save" (Just $ MkMenuAccelerator [KMCtrl] 'S') saveAction
                                  , simpleActionMenuItem "Revert" Nothing revertAction
                                  , simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') uiWindowClose
                                  , SeparatorMenuEntry
                                  , simpleActionMenuItem
                                        "Exit"
                                        (Just $ MkMenuAccelerator [KMCtrl] 'Q')
                                        uitCloseAllWindows
                                  ]
                            , SubMenuEntry
                                  "Edit"
                                  [ simpleActionMenuItem "Undo" (Just $ MkMenuAccelerator [KMCtrl] 'Z') $
                                    undo noEditSource >> return ()
                                  , simpleActionMenuItem "Redo" (Just $ MkMenuAccelerator [KMCtrl] 'Y') $
                                    redo noEditSource >> return ()
                                  ]
                            ]
                        in (mbar, spec)
                action <-
                    if saveOpt
                        then do
                            (bufferSub, saveActions) <- makeSharedSubscriber async $ saveBufferObject wholeTextObj
                            (textSub, undoActions) <- undoQueueSubscriber bufferSub
                            return $
                                makeWindow (fromString $ takeFileName path) textSub $ extraUI saveActions undoActions
                        else do
                            let
                                textObj :: Object (OneWholeEdit (Result Text) (StringEdit Text))
                                textObj = convertObject wholeTextObj
                            textSub <- makeObjectSubscriber async textObj
                            return $ makeWindow (fromString $ takeFileName path) textSub simpleUI
                action
                if double
                    then action
                    else return ()
