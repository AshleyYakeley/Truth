module Data.Changes.File.Linux
(
	withINotifyB,linuxFileObject
) where
{
	import Data.Changes;
	import System.INotify.Balanced;
	import System.Posix.Files;
	import Data.ByteString;
	import Control.Concurrent.QSem;
	import Control.Concurrent.MVar;
	import Control.Exception hiding (catch);
	import System.IO.Error;
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
		sem <- newQSem 1;
		withWatch inotify [Close] path 
		 (\_ -> signalQSem sem)
		 (loop (do
		 {
		 	waitQSem sem;
		 	f;
		 }));
	};
	
	waitOpenFileRead :: INotifyB -> FilePath -> IO (Maybe Handle);
	waitOpenFileRead inotify path = loopOnCloseWatch inotify path (catch 
	 (fmap (Just . Just) (openFile path ReadMode))
	 (\ioerror -> if isDoesNotExistError ioerror
	  then return (Just Nothing)
	  else if isAlreadyInUseError ioerror
	  then return Nothing
	  else ioError ioerror)
	 );
	
	waitOpenFileReadWrite :: INotifyB -> FilePath -> IO Handle;
	waitOpenFileReadWrite inotify path = loopOnCloseWatch inotify path (catch 
	 (fmap Just (openFile path ReadWriteMode))
	 (\ioerror -> if isAlreadyInUseError ioerror
	  then return Nothing
	  else ioError ioerror)
	);
	
	data FileState = MkFileState
	{
		fsNotify :: INotifyB,
		fsPath :: FilePath,
		fsHandleVar :: MVar (Maybe Handle)
	};
	
	newFileState :: INotifyB -> FilePath -> IO FileState;
	newFileState notify path = do
	{
		var <- newMVar Nothing;
		return (MkFileState
		{
			fsNotify = notify,
			fsPath = path,
			fsHandleVar = var
		});
	};
	
	fsClose :: FileState -> IO ();
	fsClose fs = modifyMVar_ (fsHandleVar fs) (\mh -> do
	{
		case mh of
		{
			Just h -> hClose h;
			_ -> return ();
		};
		return Nothing;
	});
	
	fsWith :: Bool -> FileState -> IO r -> IO r;
	fsWith write fs f = do
	{
		opened <- modifyMVar (fsHandleVar fs) (\mh -> case mh of
		{
			Just _ -> return (mh,False);
			_ -> if write then do
			{
				h <- waitOpenFileReadWrite (fsNotify fs) (fsPath fs);
				return (Just h,True);
			}
			else do
			{
				mh' <- waitOpenFileRead (fsNotify fs) (fsPath fs);
				return (mh',True);
			};
		});
		if opened
		 then finally f (fsClose fs)
		 else f;
	};
	
	fsWithRead :: FileState -> IO r -> IO r;
	fsWithRead = fsWith False;
	
	fsWithReadWrite :: FileState -> IO r -> IO r;
	fsWithReadWrite = fsWith True;
	
	fsGet :: FileState -> IO (Maybe ByteString);
	fsGet fs = fsWithRead fs (withMVar (fsHandleVar fs) (\mh -> case mh of
	{
		Just h -> do
		{
			-- Get the contents of a h without closing it
			hSeek h AbsoluteSeek 0;
			len <- hFileSize h;
			bs <- hGet h (fromIntegral len);	
			return (Just bs);
		};
		_ -> return Nothing;
	}));
	
	fsPut :: FileState -> Maybe ByteString -> IO ();
	fsPut fs (Just bs) = fsWithReadWrite fs (modifyMVar_ (fsHandleVar fs) (\mh -> case mh of
	{
		Just h -> do
		{
			hSeek h AbsoluteSeek 0;
			hPut h bs;
			hSetFileSize h (fromIntegral (length bs));
			return (Just h);
		};
		_ -> error "missing ReadWrite handle";
	}));
	fsPut fs _ = modifyMVar_ (fsHandleVar fs) (\mh -> do
	{
		mh' <- case mh of
		{
			Just _ -> return mh;
			_ -> waitOpenFileRead (fsNotify fs) (fsPath fs);
		};
		case mh' of
		{
			Just h -> do
			{
				hClose h;
				removeLink (fsPath fs);
				return Nothing;
			};
			_ -> return Nothing;
		};
	});

	linuxFileObject :: INotifyB -> FilePath -> Object FilePath (Maybe ByteString);
	linuxFileObject inotify path = MkObject
	{
		objContext = path,
		subscribe = \getFirst getUpdate -> do
		{
			fs <- newFileState inotify path;
			(r,wd) <- fsWithRead fs (do
			{
				a <- fsGet fs;
				r <- getFirst a;
				wd <- addWatchB inotify [Modify,MoveSelf,DeleteSelf] path (\_ -> do
				{
					newa <- fsGet fs;
					getUpdate r (ReplaceEdit newa);
				});
				return (r,wd);
			});
			return (r,MkSubscription
			{
				subPush = \edit -> return (Just (do
				{
					(wdCheck,sem) <- fsWithReadWrite fs (do
					{
						newa <- case edit of
						{
							ReplaceEdit newa -> return newa;
							_ -> do
							{
								olda <- fsGet fs;
								return (applyEdit edit olda);
							};
						};
						fsPut fs newa;
						sem <- newQSem 1;
					 	waitQSem sem;
						wdCheck <- addWatchB inotify [Close] path (\_ -> signalQSem sem);
						return (wdCheck,sem);
					});
					waitQSem sem;
					removeWatchB inotify wdCheck;
				})),
				subClose = removeWatchB inotify wd
			});
		}
	};
}
