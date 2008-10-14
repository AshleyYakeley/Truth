module File where
{
	import ValueType;
	import Edit;
	import Object;
	import System.Gnome.VFS;
	import System.Gnome.VFS.Ops(unlinkFromURI);
	import System.Gnome.VFS.Types;
	import Data.ByteString;
	import Control.Concurrent.STM;
	
	refSubscribe :: (Eq a) => ValueType a -> IO a -> (a -> IO ()) -> IO (Subscription a);
	refSubscribe vt getter setter = do
	{
		initial <- getter;
		stateVar <- newTVarIO initial;
		doneVar <- newTVarIO False;
		
		return (MkSubscription
		{
			subInitial = initial,

			subPull = atomically (do
			{
				done <- readTVar doneVar;
				if (done)
				then return Nothing
				else retry;
			}),

			subPush = \edits -> do
			{
				current <- getter;
				rr <- atomically (do
				{
					old <- readTVar stateVar;
					case compareEdit vt old current of
					{
						[] -> do
						{
							let {new = applyEdits edits old;};
							writeTVar stateVar new;
							return (Right new);
						};
						curEdits -> do
						{
							writeTVar stateVar current;
							return (Left curEdits);
						};
					};
					
				});
				case rr of
				{
					Left curEdits -> return (Just curEdits);
					Right new -> do
					{
						setter new;
						return Nothing;
					};
				};
			},

			subClose = atomically (writeTVar doneVar True)
		});
	};
	
	uriObject :: URI -> Object (Maybe ByteString);
	uriObject uri = MkObject
	{
		objContext = uri,
		subscribe = refSubscribe (MaybeValueType ByteStringValueType) getURI setURI
	} where
	{
		getURI :: IO (Maybe ByteString);
		getURI = do
		{
			catch (do
			{
				h <- openURI uri OpenRead;
				info <- getFileInfoFromHandle h [FileInfoDefault];
				Just fs <- return (fileInfoSize info);
				bs <- System.Gnome.VFS.read h fs;
				close h;
				return (Just bs);
			})
			(\_ -> return Nothing);
		};
		
		setURI :: Maybe ByteString -> IO ();
		setURI Nothing = unlinkFromURI uri;
		setURI (Just bs) = do
		{
			h <- openURI uri OpenWrite;
			write h bs;
			close h;
		};
	};
}
