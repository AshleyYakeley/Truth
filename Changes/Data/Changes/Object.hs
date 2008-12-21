module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.STM;
	import Data.Traversable;
	
	data Subscription a = MkSubscription
	{
		-- change the object. Returned result is Nothing if the changes are impossible. 
		-- If it's possible, action to change the object.
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

	{-
	modifyTVar :: TVar a -> (a -> a) -> STM ();
	modifyTVar tv mp = do
	{
		a <- readTVar tv;
		writeTVar tv (mp a);
	};
	
	writeListTChan :: TChan a -> [a] -> STM ();
	writeListTChan _ [] = return ();
	writeListTChan chan (a:as) = do
	{
		writeTChan chan a;
		writeListTChan chan as;
	};
	
	stmMaybe :: STM a -> STM (Maybe a);
	stmMaybe ma = orElse (do
	{
		a <- ma;
		return (Just a);
	}) (return Nothing);
	
	readMaybeTChan :: TChan a -> STM (Maybe a);
	readMaybeTChan chan = stmMaybe (readTChan chan);
	
	collectStream :: (Monad m) => m (Maybe a) -> m [a];
	collectStream mma = do
	{
		ma <- mma;
		case ma of
		{
			(Just a) -> do
			{
				as <- collectStream mma;
				return (a:as);
			};
			_ -> return [];
		};
	};
	
	readListTChan :: TChan a -> STM [a];
	readListTChan chan = collectStream (readMaybeTChan chan);
-}

	lockTVar :: TVar Bool -> STM a -> STM a;
	lockTVar lock action = do
	{
		locked <- readTVar lock;
		if locked
		 then retry
		 else do
		{
			a <- action;
			writeTVar lock True;
			return a;
		};
	};

	unlockTVar :: TVar Bool -> STM a -> STM a;
	unlockTVar lock action = do
	{
		writeTVar lock False;
		action;
	};

	makeFreeObject :: context -> a -> IO (Object context a);
	makeFreeObject context initial = do
	{
		stateVar <- newTVarIO initial;
		updatesVar <- newTVarIO emptyStore;
		lockVar <- newTVarIO False;
		return MkObject
		{
			objContext = context,

			subscribe = \initialise updater -> do
			{
				a <- atomically (lockTVar lockVar (readTVar stateVar));
				r <- initialise a;
				key <- atomically (unlockTVar lockVar (do
				{
					store <- readTVar updatesVar;
					let {(key,newstore) = addStore (updater r) store;};
					writeTVar updatesVar newstore;
					return key;
				}));
				return (r,MkSubscription
				{
					subPush = \edit -> return (Just (do
					{
						updaters <- atomically (lockTVar lockVar (fmap allStore (readTVar updatesVar)));
						forM updaters (\u -> u edit);
						atomically (unlockTVar lockVar (return ()));
					})),
					subClose = atomically (do
					{
						store <- readTVar updatesVar;
						writeTVar updatesVar (deleteStore key store);					
					})
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
