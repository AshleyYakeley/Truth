module Soup.UI
    ( PossibleNoteUpdate
    , soupWindow
    ) where

import Shapes
import Soup.Edit
import Soup.Note
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.World.FileSystem

fromResult :: Result Text Text -> (Text, TableCellProps)
fromResult (SuccessResult "") = ("unnamed", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
fromResult (SuccessResult s) = (s, plainTableCellProps)
fromResult (FailureResult s) = ("<" <> s <> ">", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})

pastResult :: Result Text Bool -> (Text, TableCellProps)
pastResult (SuccessResult False) = ("current", plainTableCellProps)
pastResult (SuccessResult True) = ("past", plainTableCellProps)
pastResult (FailureResult s) = ("<" <> s <> ">", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})

type PossibleNoteUpdate = FullResultOneUpdate (Result Text) NoteUpdate

soupEditSpec ::
       Model (SoupUpdate PossibleNoteUpdate)
    -> SelectNotify (Model (UUIDElementUpdate PossibleNoteUpdate))
    -> (Model (UUIDElementUpdate PossibleNoteUpdate) -> View ())
    -> CVUISpec
soupEditSpec sub selnotify openItem = do
    let
        nameLens :: ChangeLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Result Text Text))
        nameLens =
            convertReadOnlyChangeLens .
            liftFullResultOneChangeLens (tupleChangeLens NoteTitle) . tupleChangeLens SelectSecond
        cmp :: Result Text Text -> Result Text Text -> Ordering
        cmp a b = compare (resultToMaybe a) (resultToMaybe b)
        uo :: UpdateOrder (UUIDElementUpdate PossibleNoteUpdate)
        uo = MkUpdateOrder cmp $ changeLensToFloating nameLens
    osub :: Model (OrderedListUpdate [(UUID, Result Text (Tuple NoteSel))] (UUIDElementUpdate PossibleNoteUpdate)) <-
        cvFloatMapModel (orderedSetLens uo) sub
    let
        nameColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        nameColumn =
            readOnlyKeyColumn (constantModel "Name") $ \cellsub -> let
                valLens :: ChangeLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Text, TableCellProps))
                valLens =
                    funcChangeLens fromResult .
                    liftFullResultOneChangeLens (tupleChangeLens NoteTitle) . tupleChangeLens SelectSecond
                in return $ mapModel valLens cellsub {-(updateFunctionToChangeLens (funcChangeLens fromResult) . valLens)-}
        pastColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        pastColumn =
            readOnlyKeyColumn (constantModel "Past") $ \cellsub -> let
                valLens =
                    funcChangeLens pastResult .
                    liftFullResultOneChangeLens (tupleChangeLens NotePast) . tupleChangeLens SelectSecond
                in return $ mapModel valLens cellsub
    tableUISpec [nameColumn, pastColumn] osub openItem selnotify

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
    paste s = return $ getMaybeOne $ injBackwards soupItemInjection s
    soupItemLens :: ChangeLens ByteStringUpdate PossibleNoteUpdate
    soupItemLens = convertChangeLens . (wholeChangeLens $ injectionLens soupItemInjection) . convertChangeLens
    lens :: ChangeLens ReferenceSoupUpdate (SoupUpdate PossibleNoteUpdate)
    lens = liftSoupLens paste $ soupItemLens . referenceChangeLens
    in mapReference lens rawSoupReference

soupWindow :: UIToolkit -> FilePath -> CreateView ()
soupWindow uit dirpath = do
    smodel <- liftLifeCycleIO $ makeReflectingModel $ soupReference dirpath
    (selModel, selnotify) <- liftLifeCycleIO $ makeSharedModel makePremodelSelectNotify
    rec
        let
            withSelection :: (Model (UUIDElementUpdate PossibleNoteUpdate) -> View ()) -> View ()
            withSelection call = do
                msel <- viewRunResource selModel $ \selAModel -> aModelRead selAModel ReadWhole
                case msel of
                    Just sel -> call sel
                    Nothing -> return ()
            blankNote :: Tuple NoteSel
            blankNote =
                MkTuple $ \case
                    NoteTitle -> "untitled"
                    NotePast -> False
                    NoteText -> ""
            newItem :: View ()
            newItem =
                viewRunResource smodel $ \samodel -> do
                    key <- liftIO randomIO
                    _ <-
                        pushEdit noEditSource $ aModelEdit samodel $ pure $ KeyEditInsertReplace (key, return blankNote)
                    return ()
            deleteItem :: Model (UUIDElementUpdate PossibleNoteUpdate) -> View ()
            deleteItem imodel =
                viewRunResourceContext imodel $ \unlift iamodel -> do
                    key <- liftIO $ unlift $ aModelRead iamodel $ MkTupleUpdateReader SelectFirst ReadWhole
                    viewRunResource smodel $ \samodel -> do
                        _ <- pushEdit noEditSource $ aModelEdit samodel $ pure $ KeyEditDelete key
                        return ()
            mbar :: IO () -> UIWindow -> Maybe (Model (ROWUpdate [MenuEntry]))
            mbar cc _ =
                Just $
                constantModel $
                [ SubMenuEntry
                      "File"
                      [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ liftIO cc
                      , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') viewExit
                      ]
                , SubMenuEntry
                      "Item"
                      [ simpleActionMenuItem "New" (Just $ MkMenuAccelerator [KMCtrl] 'K') newItem
                      , simpleActionMenuItem "Delete" Nothing $ withSelection deleteItem
                      ]
                ]
            wsTitle :: Model (ROWUpdate Text)
            wsTitle = constantModel $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            openItem :: Model (UUIDElementUpdate PossibleNoteUpdate) -> View ()
            openItem imodel = do
                let
                    rowmodel :: Model PossibleNoteUpdate
                    rowmodel = mapModel (tupleChangeLens SelectSecond) imodel
                    rspec :: Result Text (Model NoteUpdate) -> CVUISpec
                    rspec (SuccessResult s2) = noteEditSpec s2 mempty
                    rspec (FailureResult err) = labelUISpec $ constantModel err
                rec
                    ~(subwin, subcloser) <-
                        uitUnliftCreateView uit $
                        cvEarlyCloser $
                        uitCreateWindow uit $
                        MkWindowSpec (liftIO subcloser) (constantModel "item") (mbar subcloser subwin) $
                        oneWholeUISpec rowmodel rspec
                return ()
            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
            wsMenuBar = mbar closer window
            wsContent :: CVUISpec
            wsContent =
                verticalUISpec
                    [ (False, simpleButtonUISpec (constantModel "View") $ withSelection openItem)
                    , (True, soupEditSpec smodel selnotify openItem)
                    ]
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = liftIO closer
        (window, closer) <- cvEarlyCloser $ uitCreateWindow uit MkWindowSpec {..}
    return ()
