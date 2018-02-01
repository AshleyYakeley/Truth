module Truth.World.FileSystem where

import System.Directory
import System.FilePath
import Truth.Core
import Truth.Core.Import
import Truth.World.File

-- | an entire file system in memory
type FileSystem = FileSystemDirectory

type FileSystemDirectory = [(String, FileSystemItem)]

data FileSystemItem
    = FileItem ByteString
    | DirectoryItem FileSystemDirectory
    | SymbolicLinkItem FilePath
    | OtherItem

findInFileSystem :: FileSystem -> FilePath -> Maybe FileSystemItem
findInFileSystem fs path = let
    finditem item [] = Just item
    finditem (DirectoryItem dir) (n:names) = do
        item <- lookup n dir
        finditem item names
    finditem _ _ = Nothing
    in finditem (DirectoryItem fs) $ splitDirectories path

data FSItem
    = FSFileItem (Object ByteStringEdit)
    | FSDirectoryItem
    | FSOtherItem

data FSReader t where
    FSReadDirectory :: FilePath -> FSReader (Maybe [String])
    FSReadItem :: FilePath -> FSReader (Maybe FSItem)
    FSReadSymbolicLink :: FilePath -> FSReader (Maybe String)

instance SubjectReader FSReader where
    type ReaderSubject FSReader = FileSystem
    subjectToRead fs (FSReadDirectory path) =
        case findInFileSystem fs path of
            Just (DirectoryItem items) -> Just $ fmap fst items
            _ -> Nothing
    subjectToRead fs (FSReadItem path) =
        case findInFileSystem fs path of
            Just (DirectoryItem _) -> Just FSDirectoryItem
            Just (FileItem bs) -> Just $ FSFileItem $ constantObject bs
            Just (SymbolicLinkItem sympath) -> subjectToRead fs (FSReadItem sympath)
            Just OtherItem -> Just $ FSOtherItem
            Nothing -> Nothing
    subjectToRead fs (FSReadSymbolicLink path) =
        case findInFileSystem fs path of
            Just (SymbolicLinkItem sympath) -> Just sympath
            _ -> Nothing

data FSEdit
    = FSEditCreateDirectory FilePath
    | FSEditCreateFile FilePath
                       ByteString
    | FSEditCreateSymbolicLink FilePath
                               FilePath
    | FSEditDeleteNonDirectory FilePath
    | FSEditDeleteEmptyDirectory FilePath
    | FSEditRenameItem FilePath
                       FilePath

instance Floating FSEdit FSEdit

instance Edit FSEdit where
    type EditReader FSEdit = FSReader
    applyEdit _ _ = undefined -- TODO

createFile :: FilePath -> ByteString -> IO ()
createFile path bs = do
    h <- openFile path WriteMode
    hPut h bs
    hClose h

fileSystemObject :: Object FSEdit
fileSystemObject = let
    objRun :: UnliftIO IO
    objRun = MkUnliftIO id
    objRead :: MutableRead IO FSReader
    objRead (FSReadDirectory path) = do
        isDir <- doesDirectoryExist path
        if isDir
            then do
                names <- listDirectory path
                return $ Just names
            else return Nothing
    objRead (FSReadItem path) = do
        isFile <- doesFileExist path
        if isFile
            then return $ Just $ FSFileItem $ fileObject path
            else do
                isDir <- doesDirectoryExist path
                if isDir
                    then return $ Just FSDirectoryItem
                    else do
                        exists <- doesPathExist path
                        if not exists
                            then return Nothing
                            else return $ Just FSOtherItem
    objRead (FSReadSymbolicLink path) = do
        isSymLink <- pathIsSymbolicLink path
        if isSymLink
            then fmap Just $ getSymbolicLinkTarget path
            else return Nothing
    ifMissing :: FilePath -> IO () -> IO (Maybe (IO ()))
    ifMissing path action = testEditAction (fmap not $ doesPathExist path) action
    objEdit :: [FSEdit] -> IO (Maybe (IO ()))
    objEdit =
        singleEdit $ \edit ->
            case edit of
                FSEditCreateDirectory path -> ifMissing path $ createDirectory path
                FSEditCreateFile path bs -> ifMissing path $ createFile path bs
                FSEditCreateSymbolicLink path target -> ifMissing path $ createFileLink target path
                FSEditDeleteNonDirectory path ->
                    testEditAction ((&&) <$> doesPathExist path <*> fmap not (doesDirectoryExist path)) $
                    removeFile path
                FSEditDeleteEmptyDirectory path -> testEditAction (doesDirectoryExist path) $ removeDirectory path
                FSEditRenameItem fromPath toPath ->
                    testEditAction ((&&) <$> doesPathExist fromPath <*> fmap not (doesPathExist toPath)) $
                    renamePath fromPath toPath
    in MkObject {..}
