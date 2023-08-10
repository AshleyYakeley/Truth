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

newtype AppUI =
    MkAppUI (GViewState 'Locked -> UIWindow -> GView 'Locked Widget -> (MenuBar, GView 'Locked Widget))

main :: IO ()
main = do
    (paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    runLifecycle $
        runGTK $ \gtkContext ->
            runNewView $
            runGView gtkContext $
            gvRunLocked $ do
                for_ paths $ \path -> do
                    let
                        bsObj :: Reference ByteStringEdit
                        bsObj = fileReference path
                        wholeTextObj :: Reference (WholeEdit ((Result Text) Text))
                        wholeTextObj = mapReference textLens bsObj
                        ui :: Model (FullResultOneUpdate (Result Text) (StringUpdate Text))
                           -> Maybe (Model (FullResultOneUpdate (Result Text) (StringUpdate Text)))
                           -> (GViewState 'Locked -> UIWindow -> GView 'Locked Widget -> (MenuBar, GView 'Locked Widget))
                           -> GView 'Locked Widget
                        ui sub1 msub2 extraui = do
                            (selModel, setsel) <- gvLiftLifecycleNoUI $ makeSharedModel makePremodelSelectNotify
                            let
                                openSelection ::
                                       Model (FullResultOneUpdate (Result Text) (StringUpdate Text)) -> GView 'Locked ()
                                openSelection sub = do
                                    mflens <- gvRunResource selModel $ \selAModel -> aModelRead selAModel ReadWhole
                                    case mflens of
                                        Nothing -> return ()
                                        Just flens -> do
                                            subSub <-
                                                gvLiftViewNoUI $
                                                viewFloatMapModel (liftFullResultOneFloatingChangeLens flens) sub
                                            makeWindow "section" subSub Nothing extraui
                                rTextSpec :: Result Text (Model (StringUpdate Text)) -> GView 'Locked Widget
                                rTextSpec (SuccessResult sub) = createTextArea sub setsel
                                rTextSpec (FailureResult err) = createLabel $ constantModel err
                                makeSpecs sub = do
                                    viewButton <-
                                        createButton (constantModel "GView 'Locked") $
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
                            -> (GViewState 'Locked -> UIWindow -> GView 'Locked Widget -> ( MenuBar
                                                                                          , GView 'Locked Widget))
                            -> GView 'Locked ()
                        makeWindow title sub msub2 extraui = do
                            rec
                                let (mbar, cuic) = extraui closer r $ ui sub msub2 extraui
                                (r, closer) <-
                                    gvGetState $
                                    createWindow $ let
                                        wsPosition = WindowPositionCenter
                                        wsSize = (300, 400)
                                        wsCloseBoxAction :: GView 'Locked ()
                                        wsCloseBoxAction = gvCloseState closer
                                        wsTitle :: Model (ROWUpdate Text)
                                        wsTitle = constantModel title
                                        wsContent :: AccelGroup -> GView 'Locked Widget
                                        wsContent ag = do
                                            mb <- createMenuBar ag mbar
                                            uic <- cuic
                                            createLayout
                                                OrientationVertical
                                                [ (defaultLayoutOptions, mb)
                                                , (defaultLayoutOptions {loGrow = True}, uic)
                                                ]
                                        in MkWindowSpec {..}
                            return ()
                        simpleUI ::
                               GViewState 'Locked -> UIWindow -> GView 'Locked Widget -> (MenuBar, GView 'Locked Widget)
                        simpleUI closer _ spec = let
                            mbar :: MenuBar
                            mbar =
                                [ SubMenuEntry
                                      "File"
                                      [ simpleActionMenuEntry "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $
                                        gvCloseState closer
                                      , SeparatorMenuEntry
                                      , simpleActionMenuEntry "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') gvExitUI
                                      ]
                                ]
                            in (mbar, spec)
                        extraUI ::
                               SaveActions
                            -> UndoHandler
                            -> GViewState 'Locked
                            -> UIWindow
                            -> GView 'Locked Widget
                            -> (MenuBar, GView 'Locked Widget)
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
                                      [ simpleActionMenuEntry "Save" (Just $ MkMenuAccelerator [KMCtrl] 'S') $
                                        liftIO saveAction
                                      , simpleActionMenuEntry "Revert" Nothing $ liftIO revertAction
                                      , simpleActionMenuEntry "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $
                                        gvCloseState closer
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
                            in (mbar, spec)
                    action <- do
                        (textSub, MkAppUI appUI) <-
                            if saveOpt
                                then do
                                    (bufferSub, saveActions) <-
                                        gvLiftLifecycleNoUI $
                                        makeSharedModel $ saveBufferReference emptyResourceContext wholeTextObj
                                    uh <- liftIO newUndoHandler
                                    return (undoHandlerModel uh bufferSub, MkAppUI $ extraUI saveActions uh)
                                else do
                                    textSub <- gvLiftLifecycleNoUI $ makeReflectingModel $ convertReference wholeTextObj
                                    return (textSub, MkAppUI simpleUI)
                        mTextSub2 <-
                            case selTest of
                                False -> return Nothing
                                True -> do
                                    bsObj2 <- liftIO $ makeMemoryReference mempty $ \_ -> True
                                    textSub2 <-
                                        gvLiftLifecycleNoUI $
                                        makeReflectingModel $
                                        convertReference $ mapReference textLens $ convertReference bsObj2
                                    return $ Just textSub2
                        return $ makeWindow (fromString $ takeFileName path) textSub mTextSub2 appUI
                    action
                    if double
                        then action
                        else return ()
