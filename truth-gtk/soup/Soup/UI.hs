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
       Subscriber (SoupUpdate PossibleNoteUpdate)
    -> SelectNotify (Subscriber (UUIDElementUpdate PossibleNoteUpdate))
    -> (Subscriber (UUIDElementUpdate PossibleNoteUpdate) -> View ())
    -> CVUISpec
soupEditSpec sub selnotify openItem = do
    let
        nameLens :: EditLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Result Text Text))
        nameLens =
            convertReadOnlyEditLens . liftFullResultOneEditLens (tupleEditLens NoteTitle) . tupleEditLens SelectSecond
        cmp :: Result Text Text -> Result Text Text -> Ordering
        cmp a b = compare (resultToMaybe a) (resultToMaybe b)
        uo :: UpdateOrder (UUIDElementUpdate PossibleNoteUpdate)
        uo = MkUpdateOrder cmp $ editLensToFloating nameLens
    osub :: Subscriber (OrderedListUpdate [(UUID, Result Text (Tuple NoteSel))] (UUIDElementUpdate PossibleNoteUpdate)) <-
        cvFloatMapSubscriber (orderedSetLens uo) sub
    let
        nameColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        nameColumn =
            readOnlyKeyColumn (constantSubscriber "Name") $ \cellsub -> let
                valLens :: EditLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Text, TableCellProps))
                valLens =
                    funcEditLens fromResult .
                    liftFullResultOneEditLens (tupleEditLens NoteTitle) . tupleEditLens SelectSecond
                in return $ mapSubscriber valLens cellsub {-(updateFunctionToEditLens (funcEditLens fromResult) . valLens)-}
        pastColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        pastColumn =
            readOnlyKeyColumn (constantSubscriber "Past") $ \cellsub -> let
                valLens =
                    funcEditLens pastResult .
                    liftFullResultOneEditLens (tupleEditLens NotePast) . tupleEditLens SelectSecond
                in return $ mapSubscriber valLens cellsub
    tableUISpec [nameColumn, pastColumn] osub openItem selnotify

soupObject :: FilePath -> Object (UpdateEdit (SoupUpdate PossibleNoteUpdate))
soupObject dirpath = let
    rawSoupObject :: Object (UpdateEdit ObjectSoupUpdate)
    rawSoupObject = directorySoup fileSystemObject dirpath
    soupItemInjection :: Injection' (Result Text) LazyByteString (UpdateSubject PossibleNoteUpdate)
    soupItemInjection = codecInjection noteCodec
    paste ::
           forall m. MonadIO m
        => UpdateSubject PossibleNoteUpdate
        -> m (Maybe LazyByteString)
    paste s = return $ getMaybeOne $ injBackwards soupItemInjection s
    soupItemLens :: EditLens ByteStringUpdate PossibleNoteUpdate
    soupItemLens = convertEditLens . (wholeEditLens $ injectionLens soupItemInjection) . convertEditLens
    lens :: EditLens ObjectSoupUpdate (SoupUpdate PossibleNoteUpdate)
    lens = liftSoupLens paste $ soupItemLens . objectEditLens
    in mapObject lens rawSoupObject

soupWindow :: UIToolkit -> FilePath -> CreateView ()
soupWindow uit@MkUIToolkit {..} dirpath = do
    smodel <- liftLifeCycleIO $ makeReflectingSubscriber $ soupObject dirpath
    (selnotify, getsel) <- liftIO $ makeRefSelectNotify
    rec
        let
            withSelection :: (Subscriber (UUIDElementUpdate PossibleNoteUpdate) -> View ()) -> View ()
            withSelection call = do
                msel <- getsel
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
                    _ <- pushEdit noEditSource $ subEdit samodel $ pure $ KeyEditInsertReplace (key, return blankNote)
                    return ()
            deleteItem :: Subscriber (UUIDElementUpdate PossibleNoteUpdate) -> View ()
            deleteItem imodel =
                viewRunResourceContext imodel $ \unlift iamodel -> do
                    key <- liftIO $ unlift $ subRead iamodel $ MkTupleUpdateReader SelectFirst ReadWhole
                    viewRunResource smodel $ \samodel -> do
                        _ <- pushEdit noEditSource $ subEdit samodel $ pure $ KeyEditDelete key
                        return ()
            mbar :: IO () -> UIWindow -> Maybe (Subscriber (ROWUpdate [MenuEntry]))
            mbar cc _ =
                Just $
                constantSubscriber $
                [ SubMenuEntry
                      "File"
                      [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ liftIO cc
                      , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') $ liftIO uitExit
                      ]
                , SubMenuEntry
                      "Item"
                      [ simpleActionMenuItem "New" (Just $ MkMenuAccelerator [KMCtrl] 'K') newItem
                      , simpleActionMenuItem "Delete" Nothing $ withSelection deleteItem
                      ]
                ]
            wsTitle :: Subscriber (ROWUpdate Text)
            wsTitle = constantSubscriber $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            openItem :: Subscriber (UUIDElementUpdate PossibleNoteUpdate) -> View ()
            openItem imodel = do
                let
                    rowmodel :: Subscriber PossibleNoteUpdate
                    rowmodel = mapSubscriber (tupleEditLens SelectSecond) imodel
                    rspec :: Result Text (Subscriber NoteUpdate) -> CVUISpec
                    rspec (SuccessResult s2) = noteEditSpec s2 mempty
                    rspec (FailureResult err) = labelUISpec $ constantSubscriber err
                rec
                    ~(subwin, subcloser) <-
                        uitUnliftCreateView uit $
                        cvEarlyCloser $
                        uitCreateWindow $
                        MkWindowSpec (liftIO subcloser) (constantSubscriber "item") (mbar subcloser subwin) $
                        oneWholeUISpec rowmodel rspec
                return ()
            wsMenuBar :: Maybe (Subscriber (ROWUpdate MenuBar))
            wsMenuBar = mbar closer window
            wsContent :: CVUISpec
            wsContent =
                verticalUISpec
                    [ (simpleButtonUISpec (constantSubscriber "View") $ withSelection openItem, False)
                    , (soupEditSpec smodel selnotify openItem, True)
                    ]
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = liftIO closer
        (window, closer) <- cvEarlyCloser $ uitCreateWindow MkWindowSpec {..}
    return ()
