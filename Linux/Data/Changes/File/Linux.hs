module Data.Changes.File.Linux
(
    withINotifyB,linuxFileObject
) where
{
    import Data.Changes;
    import System.INotify.Balanced;
    import System.Posix.Files;
    import Data.ConstFunction;
    import Data.ByteString;
    import Control.Concurrent.QSem;
    import Control.Concurrent.MVar;
    import Control.Monad.Identity;
    import Control.Monad.State;
    import Control.Monad.Error;
    import Control.Exception hiding (catch);
    import System.IO.Error hiding (try);
    import System.IO (Handle,IOMode(..),openFile,hClose,hSeek,SeekMode(AbsoluteSeek),hFileSize,hSetFileSize);
    import Prelude hiding (length,readFile,writeFile,catch);
    
    withWatch :: INotifyB -> [EventVariety] -> FilePath -> (Event -> IO ()) -> IO r -> IO r;
    withWatch inotify evvars path update f = bracket
        (addWatchB inotify evvars path update)
        (removeWatchB inotify)
        (\_ -> f);
    
    loop :: (Monad m) => m (Maybe a) -> m a;
    loop mma = do
    {
        ma <- mma;
        case ma of
        {
            Just a -> return a;
            _ -> loop mma;
        };
    };
    
    loopOnCloseWatch :: INotifyB -> FilePath -> IO (Maybe r) -> IO r;
    loopOnCloseWatch inotify path f = do
    {
        mr <- f;
        case mr of
        {
            Just r -> return r;
            _ -> do
            {
                sem <- newQSem 1;
                withWatch inotify [Close] path 
                 (\_ -> signalQSem sem)
                 (loop (do
                 {
                     waitQSem sem;
                     f;
                 }));
            };
        };
    };
    
    -- returns Nothing if busy, Just Nothing if non-existent, Just Just handle if successful
    checkOpenFileRead :: FilePath -> IO (Maybe (Maybe Handle));
    checkOpenFileRead path = catch 
     (fmap (Just . Just) (openFile path ReadMode))
     (\ioerror -> if isDoesNotExistError ioerror
      then return (Just Nothing)
      else if isAlreadyInUseError ioerror
      then return Nothing
      else ioError ioerror);
    
    waitOpenFileRead :: INotifyB -> FilePath -> IO (Maybe Handle);
    waitOpenFileRead inotify path = loopOnCloseWatch inotify path (checkOpenFileRead path);
    
    -- returns Nothing if busy, Just handle if successful
    checkOpenFileReadWrite :: FilePath -> IO (Maybe Handle);
    checkOpenFileReadWrite path = catch 
     (fmap Just (openFile path ReadWriteMode))
     (\ioerror -> if isAlreadyInUseError ioerror
      then return Nothing
      else ioError ioerror);
    
    waitOpenFileReadWrite :: INotifyB -> FilePath -> IO Handle;
    waitOpenFileReadWrite inotify path = loopOnCloseWatch inotify path (checkOpenFileReadWrite path);
    
    data FileState = MkFileState
    {
        fsNotify :: INotifyB,
        fsHandleVar :: MVar (FilePath,Maybe Handle),
        fsPushout :: ContextContentEdit (WholeEdit FilePath) (JustWholeEdit Maybe (WholeEdit ByteString)) -> IO (),
        fsWatchVar :: MVar (Maybe WatchDescriptorB)
    };
    
    runMVar :: MVar s -> StateT s IO a -> IO a;
    runMVar mvar f = modifyMVar mvar (\olds -> do
    {
        (a,news) <- runStateT f olds;
        return (news,a);
    });

    tryError :: (MonadError e m) => m a -> m (Either e a);
    tryError f = catchError (f >>= (return . Right)) (return . Left);
    
    finallyError :: (MonadError e m) => m a -> m () -> m a;
    finallyError f after = do
    {
        eea <- tryError f;
        after;
        case eea of
        {
            Right a -> return a;
            Left e -> throwError e;
        };
    };
    
    type FSAction a = (?fs :: FileState) => ErrorT IOError (StateT (FilePath,Maybe Handle) IO) a;
    
    runFSAction :: (?fs :: FileState) => FSAction a -> IO a;
    runFSAction f = do
    {
        eea <- runMVar (fsHandleVar ?fs) (runErrorT f);
        case eea of
        {
            Left e -> throw e;
            Right a -> return a;
        };
    };

    newFileState :: INotifyB -> FilePath -> (ContextContentEdit (WholeEdit FilePath) (JustWholeEdit Maybe (WholeEdit ByteString)) -> IO ()) -> IO FileState;
    newFileState notify path pushout = do
    {
        var <- newMVar (path,Nothing);
        watchVar <- newMVar Nothing;
        return (MkFileState
        {
            fsNotify = notify,
            fsHandleVar = var,
            fsPushout = pushout,
            fsWatchVar = watchVar
        });
    };
    
    fsClose :: FSAction ();
    fsClose = do
    {
        (path,mh) <- get;
        case mh of
        {
            Just h -> liftIO (hClose h);
            _ -> return ();
        };
        put (path,Nothing);
    };
    
    fsOpenHandle :: Bool -> FSAction Bool;
    fsOpenHandle write = do
    {
        (path,mh) <- get;
        (mh',opened) <- case mh of
        {
            Just _ -> return (mh, False);
            _ -> if write then do
            {
                h <- liftIO (waitOpenFileReadWrite (fsNotify ?fs) path);
                return (Just h, True);
            }
            else do
            {
                mh' <- liftIO (waitOpenFileRead (fsNotify ?fs) path);
                return (mh', True);
            };
        };
        put (path,mh');
        return opened;
    };
    
    fsAddWatch :: FSAction ();
    fsAddWatch = do
    {
        (path,mh) <- get;
        liftIO (modifyMVar_ (fsWatchVar ?fs) (\mwd -> case (mh,mwd) of
        {
            (Nothing,Just _) -> return Nothing;    -- file no longer exists, just forget wd
            (Just _,Nothing) -> do    -- file now exists, create a wd for it
            {
                wd <- addWatchB (fsNotify ?fs) [Modify,MoveSelf,DeleteSelf] path (\_ -> do
                {
                    (MkWithContext _ newa) <- runFSAction fsGet;
                    fsPushout ?fs (runIdentity (cleanLensPutEdit contentCleanLens (replaceEdit newa)));
                });
                return (Just wd);
            };
            _ -> return mwd;
        }));
    };
    
    fsRemoveWatch :: FSAction ();
    fsRemoveWatch = liftIO (modifyMVar_ (fsWatchVar ?fs) (\mwd -> case mwd of
    {
        Just wd -> do
        {
            removeWatchB (fsNotify ?fs) wd;
            return Nothing;
        };
        _ -> return Nothing;
    }));
    
    fsWithOpen :: Bool -> FSAction r -> FSAction r;
    fsWithOpen write f = do
    {
        opened <- fsOpenHandle write;
        if opened
         then finallyError f fsClose
         else f;
    };
    
    fsWith :: Bool -> Bool -> FSAction r -> FSAction r;
    fsWith write watch f = fsWithOpen write (do
    {
        if watch then do
        {
            fsAddWatch;
            f;
        }
        else do
        {
            fsRemoveWatch;
            finallyError f (fsWithOpen write fsAddWatch);
        };
    });
    
    fsWithRead :: FSAction r -> FSAction r;
    fsWithRead = fsWith False True;
    
    fsWithReadWrite :: Bool -> FSAction r -> FSAction r;
    fsWithReadWrite = fsWith True;
    
    fsGet :: FSAction (WithContext FilePath (Maybe ByteString));
    fsGet = fsWithRead (do
    {
        (path,mh) <- get;
        mbs <- case mh of
        {
            Just h -> do
            {
                -- Get the contents of a h without closing it
                liftIO (hSeek h AbsoluteSeek 0);
                len <- liftIO (hFileSize h);
                bs <- liftIO (hGet h (fromIntegral len));
                return (Just bs);
            };
            _ -> return Nothing;
        };
        return (MkWithContext path mbs);
    });
    
    fsPut :: Maybe ByteString -> FSAction ();
    fsPut (Just bs) = fsWithReadWrite False (do
    {
        (path,mh) <- get;
        case mh of
        {
            Just h -> do
            {
                liftIO (hSeek h AbsoluteSeek 0);
                liftIO (hPut h bs);
                liftIO (hSetFileSize h (fromIntegral (length bs)));
                put (path,Just h);
            };
            _ -> error "missing ReadWrite handle";
        };
    });
    fsPut _ = do
    {
        (path,mh) <- get;
        mh' <- case mh of
        {
            Just _ -> return mh;
            _ -> liftIO (waitOpenFileRead (fsNotify ?fs) path);
        };
        case mh' of
        {
            (Just h) -> do
            {
                liftIO (hClose h);
                liftIO (removeLink path);
            };
            _ -> return ();
        };
        put (path,Nothing);
    };

    linuxFileObject :: INotifyB -> FilePath ->
     Subscribe 
      (ContextContentEdit (WholeEdit FilePath) (JustWholeEdit Maybe (WholeEdit ByteString)));
    linuxFileObject inotify initialpath = objSubscribe (\pushout -> do
    {
        fs <- newFileState inotify initialpath pushout;
        let {?fs = fs;} in do
        {
            runFSAction (fsWithRead (return ()));
            return (MkObject
            {
                objGetInitial = \aior -> runFSAction (fsWithRead (do
                {
                    a <- fsGet;
                    liftIO (aior a);
                })),
                objPush = \edit -> runFSAction (do
                {
                    (mnewpath,mnewmbs) <- case edit of
                    {
                        _ -> do
                        {
                            newa <- applyConstFunctionA (applyEdit edit) fsGet;
                            return ((\(MkWithContext newpath newmbs) -> (Just newpath,Just newmbs)) newa);
                        };
                    };
                    case mnewpath of
                    {
                        Just newpath ->do
                        {
                            (oldpath,_) <- get;
                            if oldpath == newpath
                             then return ()
                             else do
                            {
                                fsClose;
                                liftIO (rename oldpath newpath);
                                put (newpath,Nothing);
                            };
                        };
                        _ -> return ();
                    };
                    case mnewmbs of
                    {
                        Just newmbs -> fsPut newmbs;
                        _ -> return ();
                    };
                    return (Just ());
                }),
                objClose = runFSAction (fsWithRead fsRemoveWatch)
            });
        };
    });
}
