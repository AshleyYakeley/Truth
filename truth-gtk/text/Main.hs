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

textLens :: EditLens ByteStringUpdate (WholeUpdate ((Result Text) Text))
textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) . convertEditLens

optParser :: O.Parser ([FilePath], Bool, Bool)
optParser = (,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "save")

ut :: UpdateTiming
ut = SynchronousUpdateTiming

main :: IO ()
main = do
    (paths, double, saveOpt) <- O.execParser (O.info optParser mempty)
    truthMainGTK $ \MkTruthContext {..} -> do
        (MkUIToolkit {..}, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
        for_ paths $ \path -> do
            let
                bsObj :: Object ByteStringEdit
                bsObj = fileObject path
                wholeTextObj :: Object (WholeEdit ((Result Text) Text))
                wholeTextObj = mapObject textLens bsObj
                ui :: Subscriber (OneWholeUpdate (Result Text) (StringUpdate Text))
                   -> (forall sel update. IO () -> UIWindow -> UISpec sel update -> (MenuBar update, UISpec sel update))
                   -> UISpec (EditLens (StringUpdate Text) (StringUpdate Text)) (OneWholeUpdate (Result Text) (StringUpdate Text))
                ui sub extraui =
                    withAspectUISpec $ \aspect -> let
                        openSelection :: IO ()
                        openSelection =
                            uitUnliftLifeCycle $ do
                                mlens <- aspect
                                case mlens of
                                    Nothing -> return ()
                                    Just lens -> do
                                        subLens <- mapSubscriber (return $ oneWholeLiftEditLens lens) sub
                                        makeWindow "section" subLens extraui
                        in verticalUISpec
                               [ (simpleButtonUISpec (constUpdateFunction "View") openSelection, False)
                               , (scrolledUISpec $ oneWholeUISpec textAreaUISpec, True)
                               ]
                makeWindow ::
                       Text
                    -> Subscriber (OneWholeUpdate (Result Text) (StringUpdate Text))
                    -> (forall sel update. IO () -> UIWindow -> UISpec sel update -> (MenuBar update, UISpec sel update))
                    -> LifeCycleIO ()
                makeWindow title sub extraui = do
                    rec
                        let (mbar, uic) = extraui closer r $ ui sub extraui
                        (r, closer) <-
                            lifeCycleEarlyCloser $
                            uitCreateWindow sub $
                            MkWindowSpec closer (constUpdateFunction title) (Just $ \_ -> constUpdateFunction mbar) uic
                    return ()
                simpleUI ::
                       forall sel update. IO () -> UIWindow -> UISpec sel update -> (MenuBar update, UISpec sel update)
                simpleUI closer _ spec = let
                    mbar :: MenuBar update
                    mbar =
                        [ SubMenuEntry
                              "File"
                              [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') closer
                              , SeparatorMenuEntry
                              , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') uitExit
                              ]
                        ]
                    in (mbar, spec)
                extraUI ::
                       forall sel update.
                       SaveActions
                    -> UndoActions
                    -> IO ()
                    -> UIWindow
                    -> UISpec sel update
                    -> (MenuBar update, UISpec sel update)
                extraUI (MkSaveActions saveActions) (MkUndoActions undo redo) closer _ spec = let
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
                              , simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') closer
                              , SeparatorMenuEntry
                              , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') uitExit
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
                        (bufferSub, saveActions) <- makeSharedSubscriber ut $ saveBufferObject wholeTextObj
                        (textSub, undoActions) <- liftIO $ undoQueueSubscriber bufferSub
                        return $ makeWindow (fromString $ takeFileName path) textSub $ extraUI saveActions undoActions
                    else do
                        let
                            textObj :: Object (OneWholeEdit (Result Text) (StringEdit Text))
                            textObj = convertObject wholeTextObj
                        textSub <- makeReflectingSubscriber ut textObj
                        return $ makeWindow (fromString $ takeFileName path) textSub simpleUI
            action
            if double
                then action
                else return ()
            liftIO checkdone
