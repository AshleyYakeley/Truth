module Changes.World.FileSystem where

import Changes.Core
import Shapes
import System.Directory
import System.FilePath

import Changes.World.File

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
    finditem (DirectoryItem dir) (n : names) = do
        item <- lookup n dir
        finditem item names
    finditem _ _ = Nothing
    in finditem (DirectoryItem fs) $ splitDirectories path

data FSItem
    = FSFileItem (Reference ByteStringEdit)
    | FSDirectoryItem
    | FSOtherItem

data FSReader t where
    FSReadDirectory :: FilePath -> FSReader (Maybe [String])
    FSReadItem :: FilePath -> FSReader (Maybe FSItem)
    FSReadSymbolicLink :: FilePath -> FSReader (Maybe FilePath)

{-
instance SubjectReader FSReader where
    type ReaderSubject FSReader = FileSystem
    subjectToRead fs (FSReadDirectory path) =
        case findInFileSystem fs path of
            Just (DirectoryItem items) -> Just $ fmap fst items
            _ -> Nothing
    subjectToRead fs (FSReadItem path) =
        case findInFileSystem fs path of
            Just (DirectoryItem _) -> Just FSDirectoryItem
            Just (FileItem bs) -> Just $ FSFileItem $ constantReference bs
            Just (SymbolicLinkItem sympath) -> subjectToRead fs (FSReadItem sympath)
            Just OtherItem -> Just $ FSOtherItem
            Nothing -> Nothing
    subjectToRead fs (FSReadSymbolicLink path) =
        case findInFileSystem fs path of
            Just (SymbolicLinkItem sympath) -> Just sympath
            _ -> Nothing
-}
data FSEdit
    = FSEditCreateDirectory FilePath
    | FSEditCreateFile
        FilePath
        LazyByteString
    | FSEditCreateSymbolicLink
        FilePath
        FilePath
    | FSEditDeleteNonDirectory FilePath
    | FSEditDeleteEmptyDirectory FilePath
    | FSEditRenameItem
        FilePath
        FilePath

instance FloatingOn FSEdit FSEdit

type instance EditReader FSEdit = FSReader

instance ApplicableEdit FSEdit where
    applyEdit _ _ = undefined -- TODO

createFile :: FilePath -> LazyByteString -> IO ()
createFile path bs = do
    h <- openFile path WriteMode
    hPut h bs
    hClose h

fileSystemReference :: Reference FSEdit
fileSystemReference = let
    refRead :: Readable IO FSReader
    refRead (FSReadDirectory path) = do
        isDir <- doesDirectoryExist path
        if isDir
            then do
                names <- listDirectory path
                return $ Just names
            else return Nothing
    refRead (FSReadItem path) = do
        isFile <- doesFileExist path
        if isFile
            then return $ Just $ FSFileItem $ fileReference path
            else do
                isDir <- doesDirectoryExist path
                if isDir
                    then return $ Just FSDirectoryItem
                    else do
                        exists <- doesPathExist path
                        if not exists
                            then return Nothing
                            else return $ Just FSOtherItem
    refRead (FSReadSymbolicLink path) = do
        isSymLink <- pathIsSymbolicLink path
        if isSymLink
            then fmap Just $ getSymbolicLinkTarget path
            else return Nothing
    refEdit :: NonEmpty FSEdit -> IO (Maybe (EditSource -> IO ()))
    refEdit =
        singleEdit $ \edit ->
            case edit of
                FSEditCreateDirectory path -> do
                    isDir <- doesDirectoryExist path
                    if isDir
                        then return $ Just $ \_ -> return ()
                        else do
                            exists <- doesPathExist path
                            if exists
                                then return Nothing
                                else return $ Just $ \_ -> createDirectory path
                FSEditCreateFile path bs ->
                    testEditAction (fmap not $ doesDirectoryExist path) $ \_ -> createFile path bs
                FSEditCreateSymbolicLink path target ->
                    testEditAction (fmap not $ doesDirectoryExist path) $ \_ -> createFileLink target path
                FSEditDeleteNonDirectory path ->
                    testEditAction (fmap not $ doesDirectoryExist path) $ \_ -> removeFile path
                FSEditDeleteEmptyDirectory path -> testEditAction (doesDirectoryExist path) $ \_ -> removeDirectory path
                FSEditRenameItem fromPath toPath ->
                    testEditAction ((&&) <$> doesPathExist fromPath <*> fmap not (doesPathExist toPath)) $ \_ ->
                        renamePath fromPath toPath
    refCommitTask = mempty
    in MkResource nilResourceRunner $ mapResource liftIO MkAReference{..}

subdirCreateWitness :: IOWitness (MVar Bool)
subdirCreateWitness = $(iowitness [t|MVar Bool|])

subdirectoryReference :: Bool -> FilePath -> Reference FSEdit -> Reference FSEdit
subdirectoryReference create dir (MkResource (rr :: ResourceRunner tt) (MkAReference rd push ctask)) =
    combineResourceRunners
        (discardingStateResourceRunner (hashOpenWitness subdirCreateWitness dir) create)
        rr
        $ \(rr' :: ResourceRunner ttab) liftState liftBase -> let
            MkAReference rdBase pushBase _ = mapResource (withReaderT liftBase) $ MkAReference rd push ctask
            pushFirst :: ReaderT (ListProduct ttab) IO ()
            pushFirst = do
                params <- ask
                liftIO
                    $ mVarRunStateT (fst $ liftState params)
                    $ do
                        c <- get
                        when c $ do
                            liftIO
                                $ runReaderT
                                    ( pushOrFail ("couldn't create directory " <> show dir) noEditSource
                                        $ pushBase
                                        $ pure
                                        $ FSEditCreateDirectory dir
                                    )
                                    params
                            put False
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
            rd' :: Readable (ReaderT (ListProduct ttab) IO) FSReader
            rd' (FSReadDirectory path) = do
                pushFirst
                rdBase $ FSReadDirectory $ insideToOutside path
            rd' (FSReadItem path) = do
                pushFirst
                rdBase $ FSReadItem $ insideToOutside path
            rd' (FSReadSymbolicLink path) = do
                pushFirst
                mspath <- rdBase $ FSReadSymbolicLink $ insideToOutside path
                return
                    $ case mspath of
                        Nothing -> Nothing
                        Just spath ->
                            Just
                                $ case outsideToInside spath of
                                    Just ipath -> ipath
                                    Nothing -> ""
            mapPath :: FSEdit -> FSEdit
            mapPath (FSEditCreateDirectory path) = FSEditCreateDirectory $ insideToOutside path
            mapPath (FSEditCreateFile path bs) = FSEditCreateFile (insideToOutside path) bs
            mapPath (FSEditCreateSymbolicLink path1 path2) =
                FSEditCreateSymbolicLink (insideToOutside path1) (insideToOutside path2)
            mapPath (FSEditDeleteNonDirectory path) = FSEditDeleteNonDirectory $ insideToOutside path
            mapPath (FSEditDeleteEmptyDirectory path) = FSEditDeleteEmptyDirectory $ insideToOutside path
            mapPath (FSEditRenameItem path1 path2) =
                FSEditRenameItem (insideToOutside path1) (insideToOutside path2)
            push' edits = do
                pushFirst
                pushBase $ fmap mapPath edits
            in MkResource rr' $ MkAReference rd' push' ctask
