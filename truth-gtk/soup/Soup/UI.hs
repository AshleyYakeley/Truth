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

type PossibleNoteUpdate = OneWholeUpdate (Result Text) NoteUpdate

soupEditSpec :: OpenSubscriber (SoupUpdate PossibleNoteUpdate) -> UISpec UUID
soupEditSpec sub = let
    nameFunction :: UUID -> ReadOnlyOpenSubscriber (WholeUpdate (Result Text Text))
    nameFunction key = let
        nameLens =
            oneWholeLiftEditLens (tupleEditLens NoteTitle) .
            mustExistOneEditLens "name" .
            oneWholeLiftEditLens (tupleEditLens SelectSecond) . stableKeyElementEditLens key
        in mapOpenSubscriber (updateFunctionToEditLens convertUpdateFunction . nameLens) sub
    nameColumn :: KeyColumn UUID
    nameColumn =
        readOnlyKeyColumn (openResource $ constantSubscriber "Name") $ \key -> let
            valLens =
                oneWholeLiftEditLens (tupleEditLens NoteTitle) .
                mustExistOneEditLens "name" .
                oneWholeLiftEditLens (tupleEditLens SelectSecond) . stableKeyElementEditLens key
            in return $ mapOpenSubscriber (updateFunctionToEditLens (funcUpdateFunction fromResult) . valLens) sub
    pastColumn :: KeyColumn UUID
    pastColumn =
        readOnlyKeyColumn (openResource $ constantSubscriber "Past") $ \key -> let
            valLens =
                oneWholeLiftEditLens (tupleEditLens NotePast) .
                mustExistOneEditLens "past" .
                oneWholeLiftEditLens (tupleEditLens SelectSecond) . stableKeyElementEditLens key
            in return $ mapOpenSubscriber (updateFunctionToEditLens (funcUpdateFunction pastResult) . valLens) sub
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
                                        subSub <- mapSubscriber (getKeyElementEditLens key) sub
                                        uitCreateWindow $
                                            MkWindowSpec
                                                subcloser
                                                (openResource $ constantSubscriber "item")
                                                (mbar subcloser subwin) $
                                            oneWholeUISpec
                                                (openResource $
                                                 mapPureSubscriber
                                                     (oneWholeLiftEditLens $ tupleEditLens SelectSecond)
                                                     subSub) $ \s1 -> oneWholeUISpec s1 noteEditSpec
                            return ()
                        Nothing -> return ()
            wsMenuBar :: Maybe (Aspect UUID -> ReadOnlyOpenSubscriber (WholeUpdate MenuBar))
            wsMenuBar = mbar closer window
            wsContent :: UISpec UUID
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
