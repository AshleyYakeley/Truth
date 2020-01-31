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

soupEditSpec :: OpenSubscriber (SoupUpdate PossibleNoteUpdate) -> LUISpec UUID
soupEditSpec sub = let
    nameFunction :: UUID -> ReadOnlyOpenSubscriber (WholeUpdate (Result Text Text))
    nameFunction key = let
        nameLens :: EditLens (SoupUpdate PossibleNoteUpdate) (ReadOnlyUpdate (WholeUpdate (Result Text Text)))
        nameLens = convertReadOnlyEditLens . liftFullResultOneEditLens (tupleEditLens NoteTitle) . soupRowLens key
        in mapOpenSubscriber nameLens sub
    nameColumn :: KeyColumn UUID
    nameColumn =
        readOnlyKeyColumn (openResource $ constantSubscriber "Name") $ \key -> let
            valLens :: EditLens (SoupUpdate PossibleNoteUpdate) (ReadOnlyUpdate (WholeUpdate (Text, TableCellProps)))
            valLens = funcEditLens fromResult . liftFullResultOneEditLens (tupleEditLens NoteTitle) . soupRowLens key
            in return $ mapOpenSubscriber valLens sub {-(updateFunctionToEditLens (funcEditLens fromResult) . valLens)-}
    pastColumn :: KeyColumn UUID
    pastColumn =
        readOnlyKeyColumn (openResource $ constantSubscriber "Past") $ \key -> let
            valLens = funcEditLens pastResult . liftFullResultOneEditLens (tupleEditLens NotePast) . soupRowLens key
            in return $ mapOpenSubscriber valLens sub
    in tableUISpec [nameColumn, pastColumn] (\a b -> compare (resultToMaybe a) (resultToMaybe b)) nameFunction sub $ \_ ->
           return ()

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
            mbar :: IO () -> UIWindow -> Maybe (Aspect sel -> ReadOnlyOpenSubscriber (WholeUpdate [MenuEntry]))
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
            wsTitle :: ReadOnlyOpenSubscriber (WholeUpdate Text)
            wsTitle = openResource $ constantSubscriber $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            openItem :: Aspect UUID -> IO ()
            openItem aspkey =
                uitUnliftLifeCycle $ do
                    mkey <- aspkey
                    case mkey of
                        Just key -> do
                            rec
                                ~(subwin, subcloser) <-
                                    lifeCycleEarlyCloser $ do
                                        subSub <- floatMapSubscriber (keyElementEditLens key) sub
                                        uitCreateWindow $
                                            MkWindowSpec
                                                subcloser
                                                (openResource $ constantSubscriber "item")
                                                (mbar subcloser subwin) $
                                            oneWholeUISpec
                                                (openResource $
                                                 mapSubscriber
                                                     (liftFullResultOneEditLens $ tupleEditLens SelectSecond)
                                                     subSub) $ \case
                                                Just s1 ->
                                                    oneWholeUISpec s1 $ \case
                                                        SuccessResult s2 -> noteEditSpec s2
                                                        FailureResult err ->
                                                            labelUISpec $ openResource $ constantSubscriber err
                                                Nothing -> nullUISpec
                            return ()
                        Nothing -> return ()
            wsMenuBar :: Maybe (Aspect UUID -> ReadOnlyOpenSubscriber (WholeUpdate MenuBar))
            wsMenuBar = mbar closer window
            wsContent :: LUISpec UUID
            wsContent =
                withAspectUISpec $ \aspect ->
                    verticalUISpec
                        [ (simpleButtonUISpec (openResource $ constantSubscriber "View") (openItem aspect), False)
                        , (soupEditSpec $ openResource sub, True)
                        ]
            wsCloseBoxAction :: IO ()
            wsCloseBoxAction = closer
        (window, closer) <- lifeCycleEarlyCloser $ uitCreateWindow MkWindowSpec {..}
    return ()
