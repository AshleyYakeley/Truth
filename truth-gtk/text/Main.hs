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

main :: IO ()
main =
    truthMain $ \args createWindow ->
        liftIO $ do
            (paths, double, saveOpt) <-
                O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
            for_ paths $ \path -> do
                let
                    bsObj :: Object ByteStringEdit
                    bsObj = fileObject path
                    wholeTextObj :: Object (WholeEdit ((Result Text) Text))
                    wholeTextObj = cacheWholeObject $ mapObject textLens bsObj
                    ui :: Subscriber (OneWholeEdit (Result Text) (StringEdit Text))
                       -> (forall sel edit. UIWindow -> UISpec sel edit -> UISpec sel edit)
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
                        -> (forall sel edit. UIWindow -> UISpec sel edit -> UISpec sel edit)
                        -> IO ()
                    makeWindow title sub extraui = do
                        rec
                            r <-
                                createWindow $
                                MkUserInterface sub $
                                MkWindowSpec (constEditFunction title) (extraui r $ ui sub extraui)
                        return ()
                    simpleUI :: forall sel edit. UIWindow -> UISpec sel edit -> UISpec sel edit
                    simpleUI ~MkUIWindow {..} spec = let
                        mbar :: [MenuEntry edit]
                        mbar = [SubMenuEntry "File" [simpleActionMenuItem "Close" Nothing uiWindowClose]]
                        in verticalUISpec [(menuBarUISpec mbar, False), (spec, True)]
                    extraUI ::
                           forall sel edit. SaveActions -> UndoActions -> UIWindow -> UISpec sel edit -> UISpec sel edit
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
                                  [ simpleActionMenuItem "Save" Nothing saveAction
                                  , simpleActionMenuItem "Revert" Nothing revertAction
                                  , simpleActionMenuItem "Close" Nothing uiWindowClose
                                  ]
                            , SubMenuEntry
                                  "Edit"
                                  [ simpleActionMenuItem "Undo" Nothing $ undo >> return ()
                                  , simpleActionMenuItem "Redo" Nothing $ redo >> return ()
                                  ]
                            ]
                        in verticalUISpec [(menuBarUISpec mbar, False), (spec, True)]
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
                            textSub <- makeObjectSubscriber textObj
                            return $ makeWindow (fromString $ takeFileName path) textSub simpleUI
                action
                if double
                    then action
                    else return ()
