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
    = FileItem LazyByteString
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
    FSReadSymbolicLink :: FilePath -> FSReader (Maybe FilePath)

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
                       LazyByteString
    | FSEditCreateSymbolicLink FilePath
                               FilePath
    | FSEditDeleteNonDirectory FilePath
    | FSEditDeleteEmptyDirectory FilePath
    | FSEditRenameItem FilePath
                       FilePath

instance Floating FSEdit FSEdit

type instance EditReader FSEdit = FSReader

instance ApplicableEdit FSEdit where
    applyEdit _ _ = undefined -- TODO

createFile :: FilePath -> LazyByteString -> IO ()
createFile path bs = do
    h <- openFile path WriteMode
    hPut h bs
    hClose h

fileSystemObject :: Object FSEdit
fileSystemObject = let
    objRun :: UnliftIO IO
    objRun = MkTransform id
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

subdirectoryObject :: Bool -> FilePath -> Object FSEdit -> Object FSEdit
subdirectoryObject create dir (MkObject (MkTransform run :: UnliftIO m) rd push) = let
    run' :: UnliftIO m
    run' =
        MkTransform $ \ma ->
            run $ do
                if create
                    then pushOrFail ("couldn't create directory " <> show dir) $ push [FSEditCreateDirectory dir]
                    else return ()
                ma
    insideToOutside :: FilePath -> FilePath
    insideToOutside path = let
        relpath = makeRelative "/" path
        in dir </> relpath
    outsideToInside :: FilePath -> Maybe FilePath
    outsideToInside path = let
        relpath = makeRelative dir $ "/" </> path
        in if isRelative relpath
               then Just relpath
               else Nothing
    rd' :: MutableRead m FSReader
    rd' (FSReadDirectory path) = rd $ FSReadDirectory $ insideToOutside path
    rd' (FSReadItem path) = rd $ FSReadItem $ insideToOutside path
    rd' (FSReadSymbolicLink path) = do
        mspath <- rd $ FSReadSymbolicLink $ insideToOutside path
        return $
            case mspath of
                Nothing -> Nothing
                Just spath ->
                    Just $
                    case outsideToInside spath of
                        Just ipath -> ipath
                        Nothing -> ""
    mapPath :: FSEdit -> FSEdit
    mapPath (FSEditCreateDirectory path) = FSEditCreateDirectory $ insideToOutside path
    mapPath (FSEditCreateFile path bs) = FSEditCreateFile (insideToOutside path) bs
    mapPath (FSEditCreateSymbolicLink path1 path2) =
        FSEditCreateSymbolicLink (insideToOutside path1) (insideToOutside path2)
    mapPath (FSEditDeleteNonDirectory path) = FSEditDeleteNonDirectory $ insideToOutside path
    mapPath (FSEditDeleteEmptyDirectory path) = FSEditDeleteEmptyDirectory $ insideToOutside path
    mapPath (FSEditRenameItem path1 path2) = FSEditRenameItem (insideToOutside path1) (insideToOutside path2)
    push' :: [FSEdit] -> m (Maybe (m ()))
    push' edits = push $ fmap mapPath edits
    in MkObject run' rd' push'
