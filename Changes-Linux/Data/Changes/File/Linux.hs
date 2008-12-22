module Data.Changes.File.Linux
(
	module Data.Changes.File.Linux,
	initINotify
) where
{
	import Data.Changes;
	import System.INotify;
	import Data.ByteString;
	import Prelude hiding (readFile,writeFile);
	
	linuxFileObject :: INotify -> FilePath -> Object FilePath ByteString;
	linuxFileObject inotify path = MkObject
	{
		objContext = path,
		subscribe = \getFirst getUpdate -> do
		{
			a <- readFile path;
			r <- getFirst a;
			wd <- addWatch inotify [Modify] path (\_ -> do
			{
				newa <- readFile path;
				getUpdate r (ReplaceEdit newa);
			});
			return (r,MkSubscription
			{
				subPush = \edit -> return (Just (do
				{
					newa <- case edit of
					{
						ReplaceEdit newa -> return newa;
						_ -> do
						{
							olda <- readFile path;
							return (applyEdit edit olda);
						};
					};
					writeFile path newa;
				})),
				subClose = removeWatch inotify wd
			});
		}
	};
}
