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

soupEditSpec :: Subscriber (SoupUpdate PossibleNoteUpdate) -> LUISpec (Subscriber PossibleNoteUpdate)
soupEditSpec sub = do
    let
        nameLens :: EditLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Result Text Text))
        nameLens =
            convertReadOnlyEditLens . liftFullResultOneEditLens (tupleEditLens NoteTitle) . tupleEditLens SelectSecond
        cmp :: Result Text Text -> Result Text Text -> Ordering
        cmp a b = compare (resultToMaybe a) (resultToMaybe b)
        uo :: UpdateOrder (UUIDElementUpdate PossibleNoteUpdate)
        uo = MkUpdateOrder cmp $ editLensToFloating nameLens
    osub :: Subscriber (OrderedListUpdate [t] (UUIDElementUpdate PossibleNoteUpdate)) <-
        floatMapSubscriber (orderedSetLens uo) sub
    let
        nameColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        nameColumn =
            readOnlyKeyColumn (openResource $ constantSubscriber "Name") $ \cellsub -> let
                valLens :: EditLens (UUIDElementUpdate PossibleNoteUpdate) (ROWUpdate (Text, TableCellProps))
                valLens =
                    funcEditLens fromResult .
                    liftFullResultOneEditLens (tupleEditLens NoteTitle) . tupleEditLens SelectSecond
                in return $ mapOpenSubscriber valLens cellsub {-(updateFunctionToEditLens (funcEditLens fromResult) . valLens)-}
        pastColumn :: KeyColumn (UUIDElementUpdate PossibleNoteUpdate)
        pastColumn =
            readOnlyKeyColumn (openResource $ constantSubscriber "Past") $ \cellsub -> let
                valLens =
                    funcEditLens pastResult .
                    liftFullResultOneEditLens (tupleEditLens NotePast) . tupleEditLens SelectSecond
                in return $ mapOpenSubscriber valLens cellsub
    mapSelectionUISpec (\s -> return $ mapSubscriber (tupleEditLens SelectSecond) s) $
        tableUISpec [nameColumn, pastColumn] (openResource osub) $ \_ -> return ()

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

soupWindow :: UIToolkit -> FilePath -> LifeCycleIO ()
soupWindow MkUIToolkit {..} dirpath = do
    sub <- makeReflectingSubscriber $ soupObject dirpath
    rec
        let
            mbar :: IO () -> UIWindow -> Maybe (Aspect sel -> OpenSubscriber (ROWUpdate [MenuEntry]))
            mbar cc _ =
                Just $ \_ ->
                    openResource $
                    constantSubscriber $
                    [ SubMenuEntry
                          "File"
                          [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ cc
                          , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') uitExit
                          ]
                    ]
            wsTitle :: OpenSubscriber (ROWUpdate Text)
            wsTitle = openResource $ constantSubscriber $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            openItem :: Aspect (Subscriber PossibleNoteUpdate) -> IO ()
            openItem aspkey =
                uitUnliftLifeCycle $ do
                    mkey <- aspkey
                    case mkey of
                        Just rowSub -> do
                            rec
                                ~(subwin, subcloser) <-
                                    lifeCycleEarlyCloser $
                                    uitCreateWindow $
                                    MkWindowSpec
                                        subcloser
                                        (openResource $ constantSubscriber "item")
                                        (mbar subcloser subwin) $
                                    oneWholeUISpec (openResource rowSub) $ \case
                                        SuccessResult s2 -> noteEditSpec s2
                                        FailureResult err -> labelUISpec $ openResource $ constantSubscriber err
                            return ()
                        Nothing -> return ()
            wsMenuBar :: Maybe (Aspect (Subscriber PossibleNoteUpdate) -> OpenSubscriber (ROWUpdate MenuBar))
            wsMenuBar = mbar closer window
            wsContent :: LUISpec (Subscriber PossibleNoteUpdate)
            wsContent =
                withAspectUISpec $ \aspect ->
                    verticalUISpec
                        [ (simpleButtonUISpec (openResource $ constantSubscriber "View") (openItem aspect), False)
                        , (soupEditSpec sub, True)
                        ]
            wsCloseBoxAction :: IO ()
            wsCloseBoxAction = closer
        (window, closer) <- lifeCycleEarlyCloser $ uitCreateWindow MkWindowSpec {..}
    return ()
