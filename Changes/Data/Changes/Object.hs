module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.STM;
	import Control.Concurrent.MVar;
	import Data.IORef;
	import Data.Traversable;
	
	data Subscription a = MkSubscription
	{
		-- change the object. Returned result is Nothing if the changes are impossible. 
		-- If it's possible, action to change the object.
		-- updates will be received before this action returns
		subPush :: Edit a -> IO (Maybe (IO ())),
		
		-- close the subscription
		subClose :: IO ()
	};
	
	data Object context a = MkObject
	{
		objContext :: context,
		subscribe :: forall r. (a -> IO r) -> (r -> Edit a -> IO ()) -> IO (r, Subscription a)
	};
	
	readObject :: Object context a -> IO a;
	readObject obj = do
	{
		(a,sub) <- subscribe obj return (\_ _ -> return ());
		subClose sub;
		return a;
	};

	writeObject :: a -> Object context a -> IO Bool;
	writeObject a obj = do
	{
		(_,sub) <- subscribe obj return (\_ _ -> return ());
		mpush <- subPush sub (ReplaceEdit a);
		success <- case mpush of
		{
			Just push -> do
			{
				push;
				return True;
			};
			_ -> return False;
		};
		subClose sub;
		return success;
	};

	makeFreeObject :: context -> a -> IO (Object context a);
	makeFreeObject context initial = do
	{
		updatesVar <- newMVar emptyStore;
		stateVar <- newIORef initial; -- this IORef is always protected by updatesVar
		return MkObject
		{
			objContext = context,

			subscribe = \initialise updater -> do
			{
				(r,key) <- modifyMVar updatesVar (\store -> do
				{
					a <- readIORef stateVar;
					r <- initialise a;
					let {(key,newstore) = addStore (updater r) store;};
					return (newstore,(r,key));
				});
				return (r,MkSubscription
				{
					subPush = \edit -> return (Just (
					 withMVar updatesVar (\store -> do
					 {
					 	a <- readIORef stateVar;
						writeIORef stateVar (applyEdit edit a); 
						forM (allStore store) (\u -> u edit);
						return ();
					 })
					)),
					subClose = modifyMVar_ updatesVar (\store -> return (deleteStore key store))
				});
			}
		};
	};

	lensObject :: (Eq state) => StateLens state a b -> (a -> IO state) -> Object context a -> Object context b;
	lensObject lens getstate obj = MkObject
	{
		objContext = objContext obj,
		subscribe = \initialise updater -> do
		{
			((var,r),sub) <- subscribe obj (\a -> do
			{
				state <- getstate a;
				r <- initialise (slensGet lens state a);
				var <- newTVarIO (a,state);
				return (var,r);
			}) (\(var,r) edita -> do
			{
				meditb <- atomically (do
				{
					(olda,oldstate) <- readTVar var;
					(newstate,meditb) <- return (slensUpdate lens olda edita oldstate);
					newa <- return (applyEdit edita olda);
					writeTVar var (newa,newstate);
					return meditb;
				});
				case meditb of
				{
					Just editb -> updater r editb;
					_ -> return ();
				}
			});
			return (r,MkSubscription
			{
				subPush = \edit -> do
				{
					(_,state) <- atomically (readTVar var);
					subPush sub (StateLensEdit lens state edit);
				},
				subClose = subClose sub
			});
		}
	};
}
