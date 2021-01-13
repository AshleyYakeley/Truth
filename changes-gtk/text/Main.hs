module Main
    ( main
    ) where

import Changes.Core
import Changes.UI.GTK
import Changes.World.Charset
import Changes.World.File
import qualified Options.Applicative as O
import Shapes
import System.FilePath hiding ((<.>))

textCodec :: ReasonCodec LazyByteString Text
textCodec = bijectionCodec packBijection . utf8Codec . bijectionCodec unpackBijection

textLens :: ChangeLens ByteStringUpdate (WholeUpdate ((Result Text) Text))
textLens = (wholeChangeLens $ injectionLens $ toInjection $ codecInjection textCodec) . convertChangeLens

optParser :: O.Parser ([FilePath], Bool, Bool, Bool)
optParser =
    (,,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "seltest") <*>
    O.switch (O.long "save")

newtype AppUI =
    MkAppUI (IO () -> UIWindow -> CreateView Widget -> (MenuBar, CreateView Widget))

main :: IO ()
main = do
    (paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    changesMainGTK $ \tc -> do
        let newWindow spec = ccExitOnClosed tc $ createWindow spec
        for_ paths $ \path -> do
            let
                bsObj :: Reference ByteStringEdit
                bsObj = fileReference path
                wholeTextObj :: Reference (WholeEdit ((Result Text) Text))
                wholeTextObj = mapReference textLens bsObj
                ui :: Model (FullResultOneUpdate (Result Text) (StringUpdate Text))
                   -> Maybe (Model (FullResultOneUpdate (Result Text) (StringUpdate Text)))
                   -> (IO () -> UIWindow -> CreateView Widget -> (MenuBar, CreateView Widget))
                   -> CreateView Widget
                ui sub1 msub2 extraui = do
                    (selModel, setsel) <- liftLifeCycle $ makeSharedModel makePremodelSelectNotify
                    let
                        openSelection :: Model (FullResultOneUpdate (Result Text) (StringUpdate Text)) -> View ()
                        openSelection sub = do
                            mflens <- viewRunResource selModel $ \selAModel -> aModelRead selAModel ReadWhole
                            case mflens of
                                Nothing -> return ()
                                Just flens ->
                                    ccUnliftCreateView tc $ do
                                        subSub <- cvFloatMapModel (liftFullResultOneFloatingChangeLens flens) sub
                                        makeWindow "section" subSub Nothing extraui
                        rTextSpec :: Result Text (Model (StringUpdate Text)) -> CreateView Widget
                        rTextSpec (SuccessResult sub) = createTextArea sub setsel
                        rTextSpec (FailureResult err) = createLabel $ constantModel err
                        makeSpecs sub = do
                            viewButton <- createButton (constantModel "View") $ constantModel $ Just $ openSelection sub
                            textContent <- createOneWhole sub rTextSpec
                            scrolledTextContent <- createScrolled textContent
                            return [(False, viewButton), (True, scrolledTextContent)]
                        allSpecs =
                            case msub2 of
                                Nothing -> makeSpecs sub1
                                Just sub2 -> liftA2 (<>) (makeSpecs sub1) (makeSpecs sub2)
                    specs <- allSpecs
                    createLayout OrientationVertical specs
                makeWindow ::
                       Text
                    -> Model (FullResultOneUpdate (Result Text) (StringUpdate Text))
                    -> Maybe (Model (FullResultOneUpdate (Result Text) (StringUpdate Text)))
                    -> (IO () -> UIWindow -> CreateView Widget -> (MenuBar, CreateView Widget))
                    -> CreateView ()
                makeWindow title sub msub2 extraui = do
                    rec
                        let (mbar, cuic) = extraui closer r $ ui sub msub2 extraui
                        (r, closer) <-
                            lifeCycleEarlyCloser $
                            newWindow $ let
                                wsPosition = WindowPositionCenter
                                wsSize = (300, 400)
                                wsCloseBoxAction :: View ()
                                wsCloseBoxAction = liftIO closer
                                wsTitle :: Model (ROWUpdate Text)
                                wsTitle = constantModel title
                                wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
                                wsMenuBar = Just $ constantModel mbar
                                wsContent :: CreateView Widget
                                wsContent = cuic
                                in MkWindowSpec {..}
                    return ()
                simpleUI :: IO () -> UIWindow -> CreateView Widget -> (MenuBar, CreateView Widget)
                simpleUI closer _ spec = let
                    mbar :: MenuBar
                    mbar =
                        [ SubMenuEntry
                              "File"
                              [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ liftIO closer
                              , SeparatorMenuEntry
                              , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') viewExit
                              ]
                        ]
                    in (mbar, spec)
                extraUI ::
                       SaveActions
                    -> UndoHandler
                    -> IO ()
                    -> UIWindow
                    -> CreateView Widget
                    -> (MenuBar, CreateView Widget)
                extraUI (MkSaveActions saveActions) uh closer _ spec = let
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
                              , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') viewExit
                              ]
                        , SubMenuEntry
                              "Edit"
                              [ simpleActionMenuItem "Undo" (Just $ MkMenuAccelerator [KMCtrl] 'Z') $
                                liftIO $ undoHandlerUndo uh emptyResourceContext noEditSource >> return ()
                              , simpleActionMenuItem "Redo" (Just $ MkMenuAccelerator [KMCtrl] 'Y') $
                                liftIO $ undoHandlerRedo uh emptyResourceContext noEditSource >> return ()
                              ]
                        ]
                    in (mbar, spec)
            action <- do
                (textSub, MkAppUI appUI) <-
                    if saveOpt
                        then do
                            (bufferSub, saveActions) <-
                                liftLifeCycle $ makeSharedModel $ saveBufferReference emptyResourceContext wholeTextObj
                            uh <- liftIO newUndoHandler
                            return (undoHandlerModel uh bufferSub, MkAppUI $ extraUI saveActions uh)
                        else do
                            textSub <- liftLifeCycle $ makeReflectingModel $ convertReference wholeTextObj
                            return (textSub, MkAppUI simpleUI)
                mTextSub2 <-
                    case selTest of
                        False -> return Nothing
                        True -> do
                            bsObj2 <- liftIO $ makeMemoryReference mempty $ \_ -> True
                            textSub2 <-
                                liftLifeCycle $
                                makeReflectingModel $ convertReference $ mapReference textLens $ convertReference bsObj2
                            return $ Just textSub2
                return $ makeWindow (fromString $ takeFileName path) textSub mTextSub2 appUI
            action
            if double
                then action
                else return ()
