module Truth.Core.Object.FileSystem where
{
    import Truth.Core.Import;
    import System.Directory;
    import System.FilePath;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;
    import Truth.Core.Object.File;

    -- an entire file system in memory
    type FileSystem = FileSystemDirectory;
    type FileSystemDirectory = [(String,FileSystemItem)];
    data FileSystemItem = FileItem ByteString | DirectoryItem FileSystemDirectory | SymbolicLinkItem FilePath | OtherItem;

    findInFileSystem :: FileSystem -> FilePath -> Maybe FileSystemItem;
    findInFileSystem fs path = let
    {
        finditem item [] = Just item;
        finditem (DirectoryItem dir) (n:names) = do
        {
            item <- lookup n dir;
            finditem item names;
        };
        finditem _ _ = Nothing;
    } in finditem (DirectoryItem fs) $ splitDirectories path;

    data FSItem =
        FSFileItem (Object ByteStringEdit) |
        FSDirectoryItem |
        FSOtherItem ;

    data FSReader t where
    {
        FSReadDirectory :: FilePath -> FSReader (Maybe [String]);
        FSReadItem :: FilePath -> FSReader (Maybe FSItem);
        FSReadSymbolicLink :: FilePath -> FSReader (Maybe String);
    };

    instance Reader FSReader where
    {
        type ReaderSubject FSReader = FileSystem;

        readFrom fs (FSReadDirectory path) = case findInFileSystem fs path of
        {
            Just (DirectoryItem items) -> Just $ fmap fst items;
            _ -> Nothing;
        };
        readFrom fs (FSReadItem path) = case findInFileSystem fs path of
        {
            Just (DirectoryItem _) -> Just FSDirectoryItem;
            Just (FileItem bs) -> Just $ FSFileItem $ nonlockingObject $ constantMutableEdit $ bs;
            Just (SymbolicLinkItem sympath) -> readFrom fs (FSReadItem sympath);
            Just OtherItem -> Just $ FSOtherItem;
            Nothing -> Nothing;
        };
        readFrom fs (FSReadSymbolicLink path) = case findInFileSystem fs path of
        {
            Just (SymbolicLinkItem sympath) -> Just sympath;
            _ -> Nothing;
        };
    };

    data FSEdit =
        FSEditCreateDirectory FilePath |
        FSEditCreateFile FilePath ByteString |
        FSEditCreateSymbolicLink FilePath FilePath |
        FSEditDeleteNonDirectory FilePath |
        FSEditDeleteEmptyDirectory FilePath |
        FSEditRenameItem FilePath FilePath ;

    instance Floating FSEdit FSEdit;
    instance Edit FSEdit where
    {
        type EditReader FSEdit = FSReader;
        applyEdit _ _ = undefined; -- TODO
        invertEdit _ = return undefined; -- TODO
    };

    createFile :: FilePath -> ByteString -> IO ();
    createFile path bs = do
    {
        h <- openFile path WriteMode;
        hPut h bs;
        hClose h;
    };

    fileSystemMutableEdit :: MutableEdit IO FSEdit;
    fileSystemMutableEdit = let
    {
        mutableRead :: MutableRead IO FSReader;
        mutableRead (FSReadDirectory path) = do
        {
            isDir <- doesDirectoryExist path;
            if isDir then do
            {
                names <- listDirectory path;
                return $ Just names;
            }
            else return Nothing;
        };
        mutableRead (FSReadItem path) = do
        {
            isFile <- doesFileExist path;
            if isFile then return $ Just $ FSFileItem $ fileObject path else do
            {
                isDir <- doesDirectoryExist path;
                if isDir then return $ Just FSDirectoryItem else do
                {
                    exists <- doesPathExist path;
                    if not exists then return Nothing else
                     return $ Just FSOtherItem;
                };
            };
        };
        mutableRead (FSReadSymbolicLink path) = do
        {
            isSymLink <- pathIsSymbolicLink path;
            if isSymLink then fmap Just $ getSymbolicLinkTarget path else return Nothing;
        };

        ifMissing :: FilePath -> IO () -> IO (Maybe (IO ()));
        ifMissing path action = testEditAction (fmap not $ doesPathExist path) action;

        mutableEdit :: [FSEdit] -> IO (Maybe (IO ()));
        mutableEdit = singleMutableEdit $ \edit -> case edit of
        {
            FSEditCreateDirectory path -> ifMissing path $ createDirectory path;
            FSEditCreateFile path bs -> ifMissing path $ createFile path bs;
            FSEditCreateSymbolicLink path target -> ifMissing path $ createFileLink target path;
            FSEditDeleteNonDirectory path -> testEditAction ((&&) <$> doesPathExist path <*> fmap not (doesDirectoryExist path)) $ removeFile path;
            FSEditDeleteEmptyDirectory path -> testEditAction (doesDirectoryExist path) $ removeDirectory path;
            FSEditRenameItem fromPath toPath -> testEditAction ((&&) <$> doesPathExist fromPath <*> fmap not (doesPathExist toPath)) $ renamePath fromPath toPath;
        };

    } in MkMutableEdit{..};
}
