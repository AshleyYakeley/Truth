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

textLens :: ChangeLens ByteStringUpdate (WholeUpdate ((Result Text) Text))
textLens = (wholeChangeLens $ injectionLens $ toInjection $ codecInjection textCodec) . convertChangeLens

optParser :: O.Parser ([FilePath], Bool, Bool, Bool)
optParser =
    (,,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "seltest") <*>
    O.switch (O.long "save")

newtype AppUI =
    MkAppUI (IO () -> UIWindow -> CVUISpec -> (MenuBar, CVUISpec))

--selRef :: IORef (Maybe (LifeCycleIO TextSelection))
--type SelectNotify sel = View (Maybe sel) -> View ()
main :: IO ()
main = do
    (paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    truthMainGTK $ \MkTruthContext {..} -> do
        (uit@MkUIToolkit {..}, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
        for_ paths $ \path -> do
            let
                bsObj :: Reference ByteStringEdit
                bsObj = fileReference path
                wholeTextObj :: Reference (WholeEdit ((Result Text) Text))
                wholeTextObj = mapReference textLens bsObj
                ui :: Model (FullResultOneUpdate (Result Text) (StringUpdate Text))
                   -> Maybe (Model (FullResultOneUpdate (Result Text) (StringUpdate Text)))
                   -> (IO () -> UIWindow -> CVUISpec -> (MenuBar, CVUISpec))
                   -> CVUISpec
                ui sub1 msub2 extraui = do
                    (setsel, getsel) <- liftIO $ makeRefSelectNotify
                    let
                        openSelection :: Model (FullResultOneUpdate (Result Text) (StringUpdate Text)) -> View ()
                        openSelection sub = do
                            mflens <- getsel
                            case mflens of
                                Nothing -> return ()
                                Just flens ->
                                    uitUnliftCreateView uit $ do
                                        subSub <- cvFloatMapModel (liftFullResultOneFloatingChangeLens flens) sub
                                        makeWindow "section" subSub Nothing extraui
                        rTextSpec :: Result Text (Model (StringUpdate Text)) -> CVUISpec
                        rTextSpec (SuccessResult sub) = textAreaUISpec sub setsel
                        rTextSpec (FailureResult err) = labelUISpec $ constantModel err
                        makeSpecs sub =
                            [ (simpleButtonUISpec (constantModel "View") $ openSelection sub, False)
                            , (scrolledUISpec $ oneWholeUISpec sub rTextSpec, True)
                            ]
                        allSpecs =
                            case msub2 of
                                Nothing -> makeSpecs sub1
                                Just sub2 -> makeSpecs sub1 <> makeSpecs sub2
                    verticalUISpec allSpecs
                makeWindow ::
                       Text
                    -> Model (FullResultOneUpdate (Result Text) (StringUpdate Text))
                    -> Maybe (Model (FullResultOneUpdate (Result Text) (StringUpdate Text)))
                    -> (IO () -> UIWindow -> CVUISpec -> (MenuBar, CVUISpec))
                    -> CreateView ()
                makeWindow title sub msub2 extraui = do
                    rec
                        let (mbar, uic) = extraui closer r $ ui sub msub2 extraui
                        (r, closer) <-
                            cvEarlyCloser $
                            uitCreateWindow $ let
                                wsCloseBoxAction :: View ()
                                wsCloseBoxAction = liftIO closer
                                wsTitle :: Model (ROWUpdate Text)
                                wsTitle = constantModel title
                                wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
                                wsMenuBar = Just $ constantModel mbar
                                wsContent :: CVUISpec
                                wsContent = uic
                                in MkWindowSpec {..}
                    return ()
                simpleUI :: IO () -> UIWindow -> CVUISpec -> (MenuBar, CVUISpec)
                simpleUI closer _ spec = let
                    mbar :: MenuBar
                    mbar =
                        [ SubMenuEntry
                              "File"
                              [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ liftIO closer
                              , SeparatorMenuEntry
                              , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') $ liftIO uitExit
                              ]
                        ]
                    in (mbar, spec)
                extraUI :: SaveActions -> UndoActions -> IO () -> UIWindow -> CVUISpec -> (MenuBar, CVUISpec)
                extraUI (MkSaveActions saveActions) (MkUndoActions undo redo) closer _ spec = let
                    saveAction = do
                        mactions <- saveActions
                        _ <-
                            case mactions of
                                Just (action, _) -> action emptyResourceContext noEditSource
                                _ -> return False
                        return ()
                    revertAction = do
                        mactions <- saveActions
                        _ <-
                            case mactions of
                                Just (_, action) -> action emptyResourceContext noEditSource
                                _ -> return False
                        return ()
                    mbar :: [MenuEntry]
                    mbar =
                        [ SubMenuEntry
                              "File"
                              [ simpleActionMenuItem "Save" (Just $ MkMenuAccelerator [KMCtrl] 'S') $ liftIO saveAction
                              , simpleActionMenuItem "Revert" Nothing $ liftIO revertAction
                              , simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ liftIO closer
                              , SeparatorMenuEntry
                              , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') $ liftIO uitExit
                              ]
                        , SubMenuEntry
                              "Edit"
                              [ simpleActionMenuItem "Undo" (Just $ MkMenuAccelerator [KMCtrl] 'Z') $
                                liftIO $ undo emptyResourceContext noEditSource >> return ()
                              , simpleActionMenuItem "Redo" (Just $ MkMenuAccelerator [KMCtrl] 'Y') $
                                liftIO $ redo emptyResourceContext noEditSource >> return ()
                              ]
                        ]
                    in (mbar, spec)
            action <- do
                (textSub, MkAppUI appUI) <-
                    if saveOpt
                        then do
                            (bufferSub, saveActions) <-
                                liftLifeCycleIO $
                                makeSharedModel $ saveBufferReference emptyResourceContext wholeTextObj
                            (textSub, undoActions) <- liftIO $ undoQueueModel bufferSub
                            return (textSub, MkAppUI $ extraUI saveActions undoActions)
                        else do
                            textSub <- liftLifeCycleIO $ makeReflectingModel $ convertReference wholeTextObj
                            return (textSub, MkAppUI simpleUI)
                mTextSub2 <-
                    case selTest of
                        False -> return Nothing
                        True -> do
                            bsObj2 <- liftIO $ makeMemoryReference mempty $ \_ -> True
                            textSub2 <-
                                liftLifeCycleIO $
                                makeReflectingModel $ convertReference $ mapReference textLens $ convertReference bsObj2
                            return $ Just textSub2
                return $ makeWindow (fromString $ takeFileName path) textSub mTextSub2 appUI
            action
            if double
                then action
                else return ()
            liftIO checkdone
