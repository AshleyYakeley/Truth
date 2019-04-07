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
    truthMain async $ \MkTruthContext {..} ->
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
                    makeWindow ::
                           Text
                        -> Subscriber (OneWholeEdit (Result Text) (StringEdit Text))
                        -> (forall sel edit. UIWindow -> UISpec sel edit -> (MenuBar edit, UISpec sel edit))
                        -> IO ()
                    makeWindow title sub extraui = do
                        rec
                            let (mbar, uic) = extraui r $ ui sub extraui
                            r <-
                                tcCreateWindow $
                                MkUserInterface sub $
                                MkWindowSpec (constEditFunction title) (Just $ constEditFunction mbar) uic
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
                                        tcCloseAllWindows
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
                                    Just (action, _) -> action
                                    _ -> return False
                            return ()
                        revertAction = do
                            mactions <- saveActions
                            _ <-
                                case mactions of
                                    Just (_, action) -> action
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
                                        tcCloseAllWindows
                                  ]
                            , SubMenuEntry
                                  "Edit"
                                  [ simpleActionMenuItem "Undo" (Just $ MkMenuAccelerator [KMCtrl] 'Z') $
                                    undo >> return ()
                                  , simpleActionMenuItem "Redo" (Just $ MkMenuAccelerator [KMCtrl] 'Y') $
                                    redo >> return ()
                                  ]
                            ]
                        in (mbar, spec)
                action <-
                    if saveOpt
                        then do
                            (bufferSub, saveActions) <- makeSharedSubscriber $ saveBufferObject wholeTextObj
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
