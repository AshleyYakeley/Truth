module Soup.UI
    ( PossibleNoteUpdate
    , soupWindow
    ) where

import Changes.Core
import Changes.World.FileSystem
import Changes.World.GNOME.GTK
import Shapes
import Soup.Edit
import Soup.Note
import System.FilePath

cellFromResult :: Result Text Text -> (Text, TableCellProps)
cellFromResult (SuccessResult "") = ("unnamed", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
cellFromResult (SuccessResult s) = (s, plainTableCellProps)
cellFromResult (FailureResult s) = ("<" <> s <> ">", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})

cellPastResult :: Result Text Bool -> (Text, TableCellProps)
cellPastResult (SuccessResult False) = ("current", plainTableCellProps)
cellPastResult (SuccessResult True) = ("past", plainTableCellProps)
cellPastResult (FailureResult s) = ("<" <> s <> ">", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})

type PossibleNoteUpdate = FullResultOneUpdate (Result Text) NoteUpdate

soupEditSpec ::
       Model (SoupUpdate PossibleNoteUpdate)
    -> SelectNotify (Model (UUIDElementUpdate PossibleNoteUpdate))
    -> (Model (UUIDElementUpdate PossibleNoteUpdate) -> GView 'Locked ())
    -> GView 'Unlocked Widget
soupEditSpec sub selnotify openItem = do
    let
        nameLens :: ChangeLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Result Text Text))
        nameLens =
            convertReadOnlyChangeLens .
            liftFullResultOneChangeLens (tupleChangeLens NoteTitle) . tupleChangeLens SelectSecond
        cmp :: Result Text Text -> Result Text Text -> Ordering
        cmp a b = compare (resultToMaybe a) (resultToMaybe b)
        uo :: UpdateOrder (UUIDElementUpdate PossibleNoteUpdate)
        uo = mkUpdateOrder cmp nameLens
    osub :: Model (OrderedListUpdate (UUIDElementUpdate PossibleNoteUpdate)) <-
        gvLiftView $ viewFloatMapModel (orderedSetLens uo) sub
    let
        nameColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        nameColumn =
            readOnlyKeyColumn (constantModel "Name") $ \cellsub -> let
                valLens :: ChangeLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Text, TableCellProps))
                valLens =
                    funcChangeLens cellFromResult .
                    liftFullResultOneChangeLens (tupleChangeLens NoteTitle) . tupleChangeLens SelectSecond
                in return $ mapModel valLens cellsub {-(updateFunctionToChangeLens (funcChangeLens cellFromResult) . valLens)-}
        pastColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        pastColumn =
            readOnlyKeyColumn (constantModel "Past") $ \cellsub -> let
                valLens =
                    funcChangeLens cellPastResult .
                    liftFullResultOneChangeLens (tupleChangeLens NotePast) . tupleChangeLens SelectSecond
                in return $ mapModel valLens cellsub
    (widget, _) <- createListTable [nameColumn, pastColumn] osub openItem selnotify
    return widget

soupReference :: FilePath -> Reference (UpdateEdit (SoupUpdate PossibleNoteUpdate))
soupReference dirpath = let
    rawSoupReference :: Reference (UpdateEdit ReferenceSoupUpdate)
    rawSoupReference = directorySoup fileSystemReference dirpath
    soupItemInjection :: Injection' (Result Text) LazyByteString (UpdateSubject PossibleNoteUpdate)
    soupItemInjection = codecInjection noteCodec
    paste ::
           forall m. MonadIO m
        => UpdateSubject PossibleNoteUpdate
        -> m (Maybe LazyByteString)
    paste s = return $ mToMaybe $ injBackwards soupItemInjection s
    soupItemLens :: ChangeLens ByteStringUpdate PossibleNoteUpdate
    soupItemLens = convertChangeLens . (wholeChangeLens $ injectionLens soupItemInjection) . convertChangeLens
    lens :: ChangeLens ReferenceSoupUpdate (SoupUpdate PossibleNoteUpdate)
    lens = liftSoupLens paste $ soupItemLens . referenceChangeLens
    in mapReference lens rawSoupReference

soupWindow :: (WindowSpec -> GView 'Unlocked UIWindow) -> FilePath -> GView 'Unlocked ()
soupWindow newWindow dirpath = do
    smodel <- gvLiftLifecycle $ makeReflectingModel $ soupReference dirpath
    (selModel, selnotify) <- gvLiftLifecycle $ makeSharedModel makePremodelSelectNotify
    let
        withSelection :: (Model (UUIDElementUpdate PossibleNoteUpdate) -> GView 'Unlocked ()) -> GView 'Unlocked ()
        withSelection call = do
            msel <- gvLiftView $ viewRunResource selModel $ \selAModel -> aModelRead selAModel ReadWhole
            case msel of
                Just sel -> call sel
                Nothing -> return ()
        blankNote :: Tuple NoteSel
        blankNote =
            MkTuple $ \case
                NoteTitle -> "untitled"
                NotePast -> False
                NoteText -> ""
        newItem :: GView 'Unlocked ()
        newItem =
            gvLiftView $
            viewRunResource smodel $ \samodel -> do
                key <- liftIO randomIO
                _ <- pushEdit noEditSource $ aModelEdit samodel $ pure $ KeyEditInsertReplace (key, return blankNote)
                return ()
        deleteItem :: Model (UUIDElementUpdate PossibleNoteUpdate) -> GView 'Unlocked ()
        deleteItem imodel =
            gvLiftView $
            viewRunResourceContext imodel $ \unlift iamodel -> do
                key <- liftIO $ unlift $ aModelRead iamodel $ MkTupleUpdateReader SelectFirst ReadWhole
                viewRunResource smodel $ \samodel -> do
                    _ <- pushEdit noEditSource $ aModelEdit samodel $ pure $ KeyEditDelete key
                    return ()
        mbar :: GViewState 'Unlocked -> UIWindow -> MenuBar
        mbar cc _ =
            [ SubMenuEntry
                  "File"
                  [ simpleActionMenuEntry "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $
                    gvRunUnlocked $ gvCloseState cc
                  , simpleActionMenuEntry "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') gvExitUI
                  ]
            , SubMenuEntry
                  "Item"
                  [ simpleActionMenuEntry "New" (Just $ MkMenuAccelerator [KMCtrl] 'K') $ gvRunUnlocked newItem
                  , simpleActionMenuEntry "Delete" Nothing $ gvRunUnlocked $ withSelection deleteItem
                  ]
            ]
        openItem :: Model (UUIDElementUpdate PossibleNoteUpdate) -> GView 'Unlocked ()
        openItem imodel = do
            let
                rowmodel :: Model PossibleNoteUpdate
                rowmodel = mapModel (tupleChangeLens SelectSecond) imodel
                rspec :: Result Text (Model NoteUpdate) -> GView 'Unlocked Widget
                rspec (SuccessResult s2) = noteEditSpec s2 mempty
                rspec (FailureResult err) = createLabel $ constantModel err
            rec
                ~(subwin, subcloser) <-
                    gvGetState $ let
                        wsPosition = WindowPositionCenter
                        wsSize = (300, 400)
                        wsCloseBoxAction = gvRunUnlocked $ gvCloseState subcloser
                        wsTitle = constantModel "item"
                        wsContent :: AccelGroup -> GView 'Unlocked Widget
                        wsContent ag = do
                            mb <- createMenuBar ag $ mbar subcloser subwin
                            uic <- createOneWhole rowmodel rspec
                            createLayout
                                OrientationVertical
                                [(defaultLayoutOptions, mb), (defaultLayoutOptions {loGrow = True}, uic)]
                        in newWindow MkWindowSpec {..}
            return ()
    rec
        let
            wsPosition = WindowPositionCenter
            wsSize = (300, 400)
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = constantModel $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            wsCloseBoxAction :: GView 'Locked ()
            wsCloseBoxAction = gvRunUnlocked $ gvCloseState closer
        button <-
            createButton (constantModel "Selection") $ constantModel $ Just $ gvRunUnlocked $ withSelection openItem
        stuff <- soupEditSpec smodel selnotify (\m -> gvRunUnlocked $ openItem m)
        let
            wsContent :: AccelGroup -> GView 'Unlocked Widget
            wsContent ag = do
                mb <- createMenuBar ag $ mbar closer window
                uic <-
                    createLayout
                        OrientationVertical
                        [(defaultLayoutOptions, button), (defaultLayoutOptions {loGrow = True}, stuff)]
                createLayout
                    OrientationVertical
                    [(defaultLayoutOptions, mb), (defaultLayoutOptions {loGrow = True}, uic)]
        (window, closer) <- gvGetState $ newWindow MkWindowSpec {..}
    return ()
