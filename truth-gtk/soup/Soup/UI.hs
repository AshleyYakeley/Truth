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

soupEditSpec :: UISpec UUID (SoupUpdate PossibleNoteUpdate)
soupEditSpec = let
    nameFunction :: UUID -> UpdateFunction (SoupUpdate PossibleNoteUpdate) (WholeUpdate (Result Text Text))
    nameFunction key =
        convertUpdateFunction .
        (editLensFunction $
         oneWholeLiftEditLens (tupleEditLens NoteTitle) .
         mustExistOneEditLens "name" . oneWholeLiftEditLens (tupleEditLens SelectSecond) . stableKeyElementEditLens key)
    nameColumn :: KeyColumn (SoupUpdate PossibleNoteUpdate) UUID
    nameColumn =
        readOnlyKeyColumn (constUpdateFunction "Name") $ \key -> do
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NoteTitle) .
                    mustExistOneEditLens "name" .
                    oneWholeLiftEditLens (tupleEditLens SelectSecond) . stableKeyElementEditLens key
            return $ funcUpdateFunction fromResult . editLensFunction valLens
    pastColumn :: KeyColumn (SoupUpdate PossibleNoteUpdate) UUID
    pastColumn =
        readOnlyKeyColumn (constUpdateFunction "Past") $ \key -> do
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NotePast) .
                    mustExistOneEditLens "past" .
                    oneWholeLiftEditLens (tupleEditLens SelectSecond) . stableKeyElementEditLens key
            return $ funcUpdateFunction pastResult . editLensFunction valLens
    in tableUISpec [nameColumn, pastColumn] (\a b -> compare (resultToMaybe a) (resultToMaybe b)) nameFunction id $ \_ ->
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

soupWindow :: UpdateTiming -> UIToolkit -> FilePath -> LifeCycleIO ()
soupWindow ut MkUIToolkit {..} dirpath = do
    sub <- makeReflectingSubscriber ut $ soupObject dirpath
    rec
        let
            mbar :: IO () -> UIWindow -> Maybe (Aspect sel -> UpdateFunction edit (WholeUpdate [MenuEntry edit]))
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
            openItem aspkey =
                uitUnliftLifeCycle $ do
                    mkey <- aspkey
                    case mkey of
                        Just key -> do
                            rec
                                ~(subwin, subcloser) <-
                                    lifeCycleEarlyCloser $ do
                                        subLens <- mapSubscriber (getKeyElementEditLens key) sub
                                        uitCreateWindow subLens $
                                            MkWindowSpec subcloser (constUpdateFunction "item") (mbar subcloser subwin) $
                                            mapUpdateUISpec (return $ oneWholeLiftEditLens $ tupleEditLens SelectSecond) $
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
