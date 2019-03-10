module Soup.UI
    ( PossibleNoteEdit
    , soupWindow
    ) where

import Shapes
import Soup.Edit
import Soup.Note
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.UI.GTK
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
    nameColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID
    nameColumn =
        readOnlyKeyColumn (constEditFunction "Name") $ \key -> do
            lens <- getKeyElementEditLens key
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NoteTitle) .
                    mustExistOneEditLens "name" . oneWholeLiftEditLens (tupleEditLens SelectSecond) . lens
            return $ funcEditFunction fromResult . editLensFunction valLens
    pastColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID
    pastColumn =
        readOnlyKeyColumn (constEditFunction "Past") $ \key -> do
            lens <- getKeyElementEditLens key
            let
                valLens =
                    oneWholeLiftEditLens (tupleEditLens NotePast) .
                    mustExistOneEditLens "past" . oneWholeLiftEditLens (tupleEditLens SelectSecond) . lens
            return $ funcEditFunction pastResult . editLensFunction valLens
    in simpleTableUISpec [nameColumn, pastColumn] $ \_ -> return ()

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

soupWindow :: (UserInterface WindowSpec -> IO UIWindow) -> FilePath -> IO ()
soupWindow createWindow dirpath = do
    sub <- makeObjectSubscriber $ soupObject dirpath
    rec
        let
            mbar w = menuBarUISpec [SubMenuEntry "File" [simpleActionMenuItem "Close" Nothing $ uiWindowClose w]]
            wsTitle = constEditFunction $ fromString $ takeFileName $ dropTrailingPathSeparator dirpath
            openItem :: Aspect UUID -> IO ()
            openItem aspkey = do
                mkey <- aspkey
                case mkey of
                    Just key -> do
                        lens <- getKeyElementEditLens key
                        rec
                            subwin <-
                                createWindow $
                                MkUserInterface (mapSubscriber lens sub) $
                                MkWindowSpec (constEditFunction "item") $
                                verticalUISpec
                                    [ (mbar subwin, False)
                                    , ( mapUISpec (oneWholeLiftEditLens $ tupleEditLens SelectSecond) $
                                        oneWholeUISpec $ oneWholeUISpec noteEditSpec
                                      , True)
                                    ]
                        return ()
                    Nothing -> return ()
            wsContent =
                withAspectUISpec $ \aspect ->
                    verticalUISpec
                        [ (mbar window, False)
                        , (simpleButtonUISpec (constEditFunction "View") (openItem aspect), False)
                        , (soupEditSpec, True)
                        ]
            userinterfaceSpecifier = MkWindowSpec {..}
            userinterfaceSubscriber = sub
        window <- createWindow $ MkUserInterface {..}
    return ()
