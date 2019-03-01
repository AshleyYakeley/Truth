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
                    ui :: Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) actions
                       -> (forall sel edit. (UIWindow, actions) -> UISpec sel edit -> UISpec sel edit)
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
                                   [ (buttonUISpec (constEditFunction "View") openSelection, False)
                                   , (scrolledUISpec $ oneWholeUISpec textAreaUISpec, True)
                                   ]
                    makeWindow ::
                           forall actions.
                           Text
                        -> Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) actions
                        -> (forall sel edit. (UIWindow, actions) -> UISpec sel edit -> UISpec sel edit)
                        -> IO ()
                    makeWindow title sub extraui = do
                        rec
                            r <-
                                createWindow $
                                MkUserInterface sub $
                                MkWindowSpec (constEditFunction title) (extraui r $ ui sub extraui)
                        return ()
                    simpleUI :: (UIWindow, ()) -> UISpec sel edit -> UISpec sel edit
                    simpleUI ~(MkUIWindow {..}, _) spec = let
                        mbar :: [MenuEntry]
                        mbar = [SubMenuEntry "File" [ActionMenuEntry "Close" Nothing uiWindowClose]]
                        in verticalUISpec [(menuBarUISpec mbar, False), (spec, True)]
                    extraUI :: (UIWindow, (((), SaveActions), UndoActions)) -> UISpec sel edit -> UISpec sel edit
                    extraUI ~(MkUIWindow {..}, ((_, MkSaveActions saveActions), MkUndoActions undo redo)) spec = let
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
                        mbar :: [MenuEntry]
                        mbar =
                            [ SubMenuEntry
                                  "File"
                                  [ ActionMenuEntry "Save" Nothing saveAction
                                  , ActionMenuEntry "Revert" Nothing revertAction
                                  , ActionMenuEntry "Close" Nothing uiWindowClose
                                  ]
                            , SubMenuEntry
                                  "Edit"
                                  [ActionMenuEntry "Undo" Nothing undo, ActionMenuEntry "Redo" Nothing redo]
                            ]
                        in verticalUISpec [(menuBarUISpec mbar, False), (spec, True)]
                action <-
                    if saveOpt
                        then do
                            let
                                baseSub :: Subscriber (WholeEdit ((Result Text) Text)) ()
                                baseSub = objectSubscriber $ pure wholeTextObj
                                bufferSub :: Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) ((), SaveActions)
                                bufferSub = saveBufferSubscriber baseSub
                                undoBufferSub ::
                                       Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) ( ((), SaveActions)
                                                                                                 , UndoActions)
                                undoBufferSub = undoQueueSubscriber bufferSub
                            textSub <- makeSharedSubscriber undoBufferSub
                            return $ makeWindow (fromString $ takeFileName path) textSub extraUI
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
