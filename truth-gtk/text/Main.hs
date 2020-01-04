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

optParser :: O.Parser ([FilePath], Bool, Bool, Bool)
optParser =
    (,,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "seltest") <*>
    O.switch (O.long "save")

newtype AppUI =
    MkAppUI (forall sel. IO () -> UIWindow -> UISpec sel -> (MenuBar, UISpec sel))

main :: IO ()
main = do
    (paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    truthMainGTK $ \MkTruthContext {..} -> do
        (MkUIToolkit {..}, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
        for_ paths $ \path -> do
            let
                bsObj :: Object ByteStringEdit
                bsObj = fileObject path
                wholeTextObj :: Object (WholeEdit ((Result Text) Text))
                wholeTextObj = mapObject textLens bsObj
                ui :: Subscriber (OneWholeUpdate (Result Text) (StringUpdate Text))
                   -> Maybe (Subscriber (OneWholeUpdate (Result Text) (StringUpdate Text)))
                   -> (forall sel. IO () -> UIWindow -> UISpec sel -> (MenuBar, UISpec sel))
                   -> UISpec TextSelection
                ui sub1 msub2 extraui =
                    withAspectUISpec $ \aspect -> let
                        openSelection :: Subscriber (OneWholeUpdate (Result Text) (StringUpdate Text)) -> IO ()
                        openSelection sub =
                            uitUnliftLifeCycle $ do
                                mllens <- aspect
                                case mllens of
                                    Nothing -> return ()
                                    Just llens -> do
                                        subSub <- mapSubscriber (fmap oneWholeLiftEditLens llens) sub
                                        makeWindow "section" subSub Nothing extraui
                        makeSpecs sub =
                            [ (simpleButtonUISpec (openResource $ constantSubscriber "View") $ openSelection sub, False)
                            , (scrolledUISpec $ oneWholeUISpec (openResource sub) textAreaUISpec, True)
                            ]
                        allSpecs =
                            case msub2 of
                                Nothing -> makeSpecs sub1
                                Just sub2 -> makeSpecs sub1 <> makeSpecs sub2
                        in verticalUISpec allSpecs
                makeWindow ::
                       Text
                    -> Subscriber (OneWholeUpdate (Result Text) (StringUpdate Text))
                    -> Maybe (Subscriber (OneWholeUpdate (Result Text) (StringUpdate Text)))
                    -> (forall sel. IO () -> UIWindow -> UISpec sel -> (MenuBar, UISpec sel))
                    -> LifeCycleIO ()
                makeWindow title sub msub2 extraui = do
                    rec
                        let (mbar, uic) = extraui closer r $ ui sub msub2 extraui
                        (r, closer) <-
                            lifeCycleEarlyCloser $
                            uitCreateWindow $
                            MkWindowSpec
                                closer
                                (openResource $ constantSubscriber title)
                                (Just $ \_ -> openResource $ constantSubscriber mbar)
                                uic
                    return ()
                simpleUI :: forall sel. IO () -> UIWindow -> UISpec sel -> (MenuBar, UISpec sel)
                simpleUI closer _ spec = let
                    mbar :: MenuBar
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
                       forall sel.
                       SaveActions
                    -> UndoActions
                    -> IO ()
                    -> UIWindow
                    -> UISpec sel
                    -> (MenuBar, UISpec sel)
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
                    mbar :: [MenuEntry]
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
            action <- do
                (textSub, MkAppUI appUI) <-
                    if saveOpt
                        then do
                            (bufferSub, saveActions) <- makeSharedSubscriber $ saveBufferObject wholeTextObj
                            (textSub, undoActions) <- liftIO $ undoQueueSubscriber bufferSub
                            return (textSub, MkAppUI $ extraUI saveActions undoActions)
                        else do
                            textSub <- makeReflectingSubscriber $ convertObject wholeTextObj
                            return (textSub, MkAppUI simpleUI)
                mTextSub2 <-
                    case selTest of
                        False -> return Nothing
                        True -> do
                            bsObj2 <- liftIO $ freeIOObject mempty $ \_ -> True
                            textSub2 <-
                                makeReflectingSubscriber $ convertObject $ mapObject textLens $ convertObject bsObj2
                            return $ Just textSub2
                return $ makeWindow (fromString $ takeFileName path) textSub mTextSub2 appUI
            action
            if double
                then action
                else return ()
            liftIO checkdone
