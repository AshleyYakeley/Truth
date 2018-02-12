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

fromResult :: Result String String -> (String, TableCellProps)
fromResult (SuccessResult "") = ("unnamed", tableCellPlain {tcItalic = True})
fromResult (SuccessResult s) = (s, tableCellPlain)
fromResult (FailureResult s) = ("<" ++ s ++ ">", tableCellPlain {tcItalic = True})

pastResult :: Result String Bool -> (String, TableCellProps)
pastResult (SuccessResult False) = ("current", tableCellPlain)
pastResult (SuccessResult True) = ("past", tableCellPlain)
pastResult (FailureResult s) = ("<" ++ s ++ ">", tableCellPlain {tcItalic = True})

type PossibleNoteEdit = OneWholeEdit (Result String) NoteEdit

soupEditSpec :: UISpec (SoupEdit PossibleNoteEdit)
soupEditSpec = let
    nameColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID
    nameColumn =
        readOnlyKeyColumn "Name" $ \key -> do
            lens <- getKeyElementEditLens key
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NoteTitle) .
                    mustExistOneEditLens "name" . oneWholeLiftEditLens (tupleEditLens EditSecond) . lens
            return $ funcEditFunction fromResult . editLensFunction valLens
    pastColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID
    pastColumn =
        readOnlyKeyColumn "Past" $ \key -> do
            lens <- getKeyElementEditLens key
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NotePast) .
                    mustExistOneEditLens "past" . oneWholeLiftEditLens (tupleEditLens EditSecond) . lens
            return $ funcEditFunction pastResult . editLensFunction valLens
    getaspect :: Aspect (MaybeEdit (UUIDElementEdit PossibleNoteEdit))
    getaspect =
        return $
        Just $ ("item", uiLens (oneWholeLiftEditLens $ tupleEditLens EditSecond) $ uiOneWhole $ uiOneWhole noteEditSpec)
    in uiSimpleTable [nameColumn, pastColumn] getaspect

soupObject :: FilePath -> Object (SoupEdit PossibleNoteEdit)
soupObject dirpath = let
    rawSoupObject :: Object (SoupEdit (ObjectEdit ByteStringEdit))
    rawSoupObject = directorySoup fileSystemObject dirpath
    soupItemInjection :: Injection' (Result String) ByteString (EditSubject PossibleNoteEdit)
    soupItemInjection = codecInjection noteCodec
    paste ::
           forall m. MonadIO m
        => EditSubject PossibleNoteEdit
        -> m (Maybe ByteString)
    paste s = return $ getMaybeOne $ injBackwards soupItemInjection s
    soupItemLens :: EditLens ByteStringEdit PossibleNoteEdit
    soupItemLens = convertEditLens . (wholeEditLens $ injectionLens soupItemInjection) . convertEditLens
    lens :: EditLens (SoupEdit (ObjectEdit ByteStringEdit)) (SoupEdit PossibleNoteEdit)
    lens = liftSoupLens paste $ soupItemLens . objectEditLens
    in mapObject lens rawSoupObject

soupWindow :: FilePath -> IO (UIWindow ())
soupWindow dirpath = do
    let
        uiwTitle = takeFileName $ dropTrailingPathSeparator dirpath
        uiwSpec = soupEditSpec
    uiwSubscriber <- makeObjectSubscriber $ soupObject dirpath
    return $ MkUIWindow {..}
