module Soup.UI
    ( PossibleNoteEdit
    , soupWindow
    ) where

import Shapes
import Soup.Edit
import Soup.Note
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.World.FileSystem

fromResult :: Result Text Text -> (Text, TableCellProps)
fromResult (SuccessResult "") = ("unnamed", tableCellPlain {tcItalic = True})
fromResult (SuccessResult s) = (s, tableCellPlain)
fromResult (FailureResult s) = ("<" <> s <> ">", tableCellPlain {tcItalic = True})

pastResult :: Result Text Bool -> (Text, TableCellProps)
pastResult (SuccessResult False) = ("current", tableCellPlain)
pastResult (SuccessResult True) = ("past", tableCellPlain)
pastResult (FailureResult s) = ("<" <> s <> ">", tableCellPlain {tcItalic = True})

type PossibleNoteEdit = OneWholeEdit (Result Text) NoteEdit

soupEditSpec :: UISpec UUID (SoupEdit PossibleNoteEdit)
soupEditSpec = let
    nameFunction :: UUID -> UpdateFunction (SoupEdit PossibleNoteEdit) (WholeEdit (Result Text Text))
    nameFunction key =
        convertUpdateFunction .
        (editLensFunction $
         oneWholeLiftEditLens (tupleEditLens NoteTitle) .
         mustExistOneEditLens "name" . oneWholeLiftEditLens (tupleEditLens SelectSecond) . stableKeyElementEditLens key)
    nameColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID
    nameColumn =
        readOnlyKeyColumn (constUpdateFunction "Name") $ \key -> do
            lens <- getKeyElementEditLens key
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NoteTitle) .
                    mustExistOneEditLens "name" . oneWholeLiftEditLens (tupleEditLens SelectSecond) . lens
            return $ funcUpdateFunction fromResult . editLensFunction valLens
    pastColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID
    pastColumn =
        readOnlyKeyColumn (constUpdateFunction "Past") $ \key -> do
            lens <- getKeyElementEditLens key
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NotePast) .
                    mustExistOneEditLens "past" . oneWholeLiftEditLens (tupleEditLens SelectSecond) . lens
            return $ funcUpdateFunction pastResult . editLensFunction valLens
    in tableUISpec [nameColumn, pastColumn] (\a b -> compare (resultToMaybe a) (resultToMaybe b)) nameFunction id $ \_ ->
           return ()

soupObject :: FilePath -> Object (SoupEdit PossibleNoteEdit)
soupObject dirpath = let
    rawSoupObject :: Object (SoupEdit (ObjectEdit ByteStringEdit))
    rawSoupObject = directorySoup fileSystemObject dirpath
    soupItemInjection :: Injection' (Result Text) LazyByteString (EditSubject PossibleNoteEdit)
    soupItemInjection = codecInjection noteCodec
    paste ::
           forall m. MonadIO m
        => EditSubject PossibleNoteEdit
        -> m (Maybe LazyByteString)
    paste s = return $ getMaybeOne $ injBackwards soupItemInjection s
    soupItemLens :: EditLens ByteStringEdit PossibleNoteEdit
    soupItemLens = convertEditLens . (wholeEditLens $ injectionLens soupItemInjection) . convertEditLens
    lens :: EditLens (SoupEdit (ObjectEdit ByteStringEdit)) (SoupEdit PossibleNoteEdit)
    lens = liftSoupLens paste $ soupItemLens . objectEditLens
    in mapObject lens rawSoupObject

soupWindow :: UpdateTiming -> UIToolkit -> FilePath -> LifeCycleIO ()
soupWindow ut MkUIToolkit {..} dirpath = do
    sub <- makeReflectingSubscriber ut $ soupObject dirpath
    rec
        let
            mbar :: IO () -> UIWindow -> Maybe (Aspect sel -> UpdateFunction edit (WholeEdit [MenuEntry edit]))
            mbar cc _ =
                Just $ \_ ->
                    constUpdateFunction $
                    [ SubMenuEntry
                          "File"
                          [ simpleActionMenuItem "Close" (Just $ MkMenuAccelerator [KMCtrl] 'W') $ cc
                          , simpleActionMenuItem "Exit" (Just $ MkMenuAccelerator [KMCtrl] 'Q') uitExit
                          ]
                    ]
            wsTitle = constUpdateFunction $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            openItem :: Aspect UUID -> IO ()
            openItem aspkey = do
                mkey <- aspkey
                case mkey of
                    Just key -> do
                        lens <- getKeyElementEditLens key
                        uitUnliftLifeCycle $ do
                            rec
                                ~(subwin, subcloser) <-
                                    lifeCycleEarlyCloser $ do
                                        subLens <- mapSubscriber lens sub
                                        uitCreateWindow subLens $
                                            MkWindowSpec subcloser (constUpdateFunction "item") (mbar subcloser subwin) $
                                            mapEditUISpec (oneWholeLiftEditLens $ tupleEditLens SelectSecond) $
                                            oneWholeUISpec $ oneWholeUISpec noteEditSpec
                            return ()
                    Nothing -> return ()
            wsMenuBar = mbar closer window
            wsContent =
                withAspectUISpec $ \aspect ->
                    verticalUISpec
                        [ (simpleButtonUISpec (constUpdateFunction "View") (openItem aspect), False)
                        , (soupEditSpec, True)
                        ]
            wsCloseBoxAction = closer
        (window, closer) <- lifeCycleEarlyCloser $ uitCreateWindow sub MkWindowSpec {..}
    return ()
