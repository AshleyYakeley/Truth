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
import Truth.Debug.Object

fromResult :: Result Text Text -> (Text, TableCellProps)
fromResult (SuccessResult "") = ("unnamed", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})
fromResult (SuccessResult s) = (s, plainTableCellProps)
fromResult (FailureResult s) = ("<" <> s <> ">", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})

pastResult :: Result Text Bool -> (Text, TableCellProps)
pastResult (SuccessResult False) = ("current", plainTableCellProps)
pastResult (SuccessResult True) = ("past", plainTableCellProps)
pastResult (FailureResult s) = ("<" <> s <> ">", plainTableCellProps {tcStyle = plainTextStyle {tsItalic = True}})

type PossibleNoteUpdate = FullResultOneUpdate (Result Text) NoteUpdate

soupEditSpec :: Subscriber (SoupUpdate PossibleNoteUpdate) -> SelectNotify (Subscriber PossibleNoteUpdate) -> CVUISpec
soupEditSpec sub selnotify = do
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
        tselnotify :: SelectNotify (Subscriber (UUIDElementUpdate PossibleNoteUpdate))
        tselnotify = contramap (\s -> mapSubscriber (tupleEditLens SelectSecond) s) selnotify
    tableUISpec [nameColumn, pastColumn] osub (\_ -> return ()) tselnotify

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
    sub <- liftLifeCycleIO $ makeReflectingSubscriber $ traceThing "soup" $ soupObject dirpath
    (selnotify, getsel) <- liftIO $ makeRefSelectNotify
    rec
        let
            mbar :: IO () -> UIWindow -> Maybe (Subscriber (ROWUpdate [MenuEntry]))
            mbar cc _ =
                Just $
                constantSubscriber $
                [ SubMenuEntry
                      "File"
                      [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ liftIO cc
                      , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') $ liftIO uitExit
                      ]
                ]
            wsTitle :: Subscriber (ROWUpdate Text)
            wsTitle = constantSubscriber $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            openItem :: View ()
            openItem = do
                mkey <- getsel
                case mkey of
                    Just rowSub -> do
                        let
                            rspec :: Result Text (Subscriber NoteUpdate) -> CVUISpec
                            rspec (SuccessResult s2) = noteEditSpec s2 mempty
                            rspec (FailureResult err) = labelUISpec $ constantSubscriber err
                        rec
                            ~(subwin, subcloser) <-
                                uitUnliftCreateView uit $
                                cvEarlyCloser $
                                    -- uitRunView uit emptyResourceContext $
                                uitCreateWindow $
                                MkWindowSpec (liftIO subcloser) (constantSubscriber "item") (mbar subcloser subwin) $
                                oneWholeUISpec rowSub rspec
                        return ()
                    Nothing -> return ()
            wsMenuBar :: Maybe (Subscriber (ROWUpdate MenuBar))
            wsMenuBar = mbar closer window
            wsContent :: CVUISpec -- (Subscriber PossibleNoteUpdate)
            wsContent =
                verticalUISpec
                    [ (simpleButtonUISpec (constantSubscriber "View") openItem, False)
                    , (soupEditSpec sub selnotify, True)
                    ]
            wsCloseBoxAction :: View ()
            wsCloseBoxAction = liftIO closer
        (window, closer) <- cvEarlyCloser $ uitCreateWindow MkWindowSpec {..}
    return ()
