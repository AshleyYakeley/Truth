module Main
    ( main
    ) where

import Changes.Core
import Changes.World.File
import Changes.World.GNOME.GTK
import qualified Options.Applicative as O
import Shapes
import System.FilePath

textCodec :: ReasonCodec LazyByteString Text
textCodec = hoistCodec (mapResultFailure $ pack . show) utf8Codec . bijectionCodec strictBytestringBijection

textLens :: ChangeLens ByteStringUpdate (WholeUpdate ((Result Text) Text))
textLens = (wholeChangeLens $ injectionLens $ toInjection $ codecInjection textCodec) . convertChangeLens

optParser :: O.Parser ([FilePath], Bool, Bool, Bool)
optParser =
    (,,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "seltest") <*>
    O.switch (O.long "save")

type AppUI = GViewState 'Unlocked -> MenuBar

main :: IO ()
main = do
    (paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    runLifecycle $
        runGTK $ \gtkContext ->
            runView $
            runGView gtkContext $ do
                for_ paths $ \path -> do
                    let
                        bsObj :: Reference ByteStringEdit
                        bsObj = fileReference path
                        wholeTextObj :: Reference (WholeEdit ((Result Text) Text))
                        wholeTextObj = mapReference textLens bsObj
                        ui :: Model (FullResultOneUpdate (Result Text) (StringUpdate Text))
                           -> Maybe (Model (FullResultOneUpdate (Result Text) (StringUpdate Text)))
                           -> AppUI
                           -> GView 'Unlocked Widget
                        ui sub1 msub2 appui = do
                            (selModel, setsel) <- gvLiftLifecycle $ makeSharedModel makePremodelSelectNotify
                            let
                                openSelection ::
                                       Model (FullResultOneUpdate (Result Text) (StringUpdate Text)) -> GView 'Locked ()
                                openSelection sub =
                                    gvRunUnlocked $ do
                                        mflens <-
                                            gvLiftView $
                                            viewRunResource selModel $ \selAModel -> aModelRead selAModel ReadWhole
                                        case mflens of
                                            Nothing -> return ()
                                            Just flens -> do
                                                subSub <-
                                                    gvLiftView $
                                                    viewFloatMapModel (liftFullResultOneFloatingChangeLens flens) sub
                                                _ <- makeWindow "section" subSub Nothing appui
                                                return ()
                                rTextSpec :: Result Text (Model (StringUpdate Text)) -> GView 'Unlocked Widget
                                rTextSpec (SuccessResult sub) = createTextView sub setsel
                                rTextSpec (FailureResult err) = createLabel $ constantModel err
                                makeSpecs sub = do
                                    viewButton <-
                                        createButton (constantModel "Open Selection") $
                                        constantModel $ Just $ openSelection sub
                                    textContent <- createOneWhole sub rTextSpec
                                    scrolledTextContent <- createScrolled textContent
                                    return
                                        [ (defaultLayoutOptions, viewButton)
                                        , (defaultLayoutOptions {loGrow = True}, scrolledTextContent)
                                        ]
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
                            -> AppUI
                            -> GView 'Unlocked UIWindow
                        makeWindow title sub msub2 appui = do
                            rec
                                (window, closer) <-
                                    gvGetState $
                                    createWindow $ let
                                        wsPosition = WindowPositionCenter
                                        wsSize = (300, 400)
                                        wsCloseBoxAction :: GView 'Locked ()
                                        wsCloseBoxAction = gvRunUnlocked $ gvCloseState closer
                                        wsTitle :: Model (ROWUpdate Text)
                                        wsTitle = constantModel title
                                        wsContent :: AccelGroup -> GView 'Unlocked Widget
                                        wsContent ag = do
                                            mb <- createMenuBar ag $ appui closer
                                            uic <- ui sub msub2 appui
                                            createLayout
                                                OrientationVertical
                                                [ (defaultLayoutOptions, mb)
                                                , (defaultLayoutOptions {loGrow = True}, uic)
                                                ]
                                        in MkWindowSpec {..}
                            return window
                        simpleUI :: AppUI
                        simpleUI closer =
                            [ SubMenuEntry
                                  "File"
                                  [ simpleActionMenuEntry "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $
                                    gvRunUnlocked $ gvCloseState closer
                                  , SeparatorMenuEntry
                                  , simpleActionMenuEntry "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') gvExitUI
                                  ]
                            ]
                        extraUI :: SaveActions -> UndoHandler -> AppUI
                        extraUI (MkSaveActions saveActions) uh closer = let
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
                            in [ SubMenuEntry
                                     "File"
                                     [ simpleActionMenuEntry "Save" (Just $ MkMenuAccelerator [KMCtrl] 'S') $
                                       liftIO saveAction
                                     , simpleActionMenuEntry "Revert" Nothing $ liftIO revertAction
                                     , simpleActionMenuEntry "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $
                                       gvRunUnlocked $ gvCloseState closer
                                     , SeparatorMenuEntry
                                     , simpleActionMenuEntry "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') gvExitUI
                                     ]
                               , SubMenuEntry
                                     "Edit"
                                     [ simpleActionMenuEntry "Undo" (Just $ MkMenuAccelerator [KMCtrl] 'Z') $
                                       liftIO $ undoHandlerUndo uh emptyResourceContext noEditSource >> return ()
                                     , simpleActionMenuEntry "Redo" (Just $ MkMenuAccelerator [KMCtrl] 'Y') $
                                       liftIO $ undoHandlerRedo uh emptyResourceContext noEditSource >> return ()
                                     ]
                               ]
                    (textSub, appUI) <-
                        if saveOpt
                            then do
                                (bufferSub, saveActions) <-
                                    gvLiftLifecycle $
                                    makeSharedModel $ saveBufferReference emptyResourceContext wholeTextObj
                                uh <- gvLiftIONoUI newUndoHandler
                                return (undoHandlerModel uh bufferSub, extraUI saveActions uh)
                            else do
                                textSub <- gvLiftLifecycle $ makeReflectingModel $ convertReference wholeTextObj
                                return (textSub, simpleUI)
                    mTextSub2 <-
                        case selTest of
                            False -> return Nothing
                            True -> do
                                bsObj2 <- gvLiftIONoUI $ makeMemoryReference mempty $ \_ -> True
                                textSub2 <-
                                    gvLiftLifecycle $
                                    makeReflectingModel $
                                    convertReference $ mapReference textLens $ convertReference bsObj2
                                return $ Just textSub2
                    let
                        action = do
                            _ <- makeWindow (fromString $ takeFileName path) textSub mTextSub2 appUI
                            return ()
                    action
                    if double
                        then action
                        else return ()
