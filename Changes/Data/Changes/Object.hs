module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.STM;
	import Control.Concurrent.MVar;
	import Data.Traversable;
	
	data Subscription token a = MkSubscription
	{
		-- change the object.
		-- returns Nothing if not synchronised.
		-- returns Just Nothing if change is not allowed
		-- returns Just (Just ()) if change succeeded, and updates will be received before this action returns
		subPush :: token -> Edit a -> IO (Maybe (Maybe ())),
		
		-- close the subscription
		subClose :: IO ()
	};
	
	data Object context a = forall token. MkObject
	{
		objContext :: context,
		objSubscribe :: forall r. (a -> IO r) -> (r -> token -> Edit a -> IO ()) -> IO (r, token, Subscription token a)
	};
	
	readObject :: Object context a -> IO a;
	readObject (MkObject _ subscribe) = do
	{
		(a,_,sub) <- subscribe return (\_ _ _ -> return ());
		subClose sub;
		return a;
	};

	writeObject :: a -> Object context a -> IO Bool;
	writeObject a (MkObject _ subscribe) = do
	{
		(_,token,sub) <- subscribe return (\_ _ _ -> return ());
		mpush <- subPush sub token (ReplaceEdit a);
		success <- return (case mpush of
		{
			Just (Just ()) -> True;
			_ -> False;
		});
		subClose sub;
		return success;
	};

	makeFreeObject :: context -> a -> IO (Object context a);
	makeFreeObject context initial = do
	{
		stateVar <- newMVar (emptyStore,initial,0 :: Integer);
		return MkObject
		{
			objContext = context,

			objSubscribe = \initialise updater -> do
			{
				(r,key) <- modifyMVar stateVar (\(store,a,token) -> do
				{
					r <- initialise a;
					let {(key,newstore) = addStore (updater r) store;};
					return ((newstore,a,token),(r,key));
				});
				return (r,0 :: Integer,MkSubscription
				{
					subPush = \edittoken edit -> modifyMVar stateVar (\(store,a,token) -> do
					{
						if edittoken == token then let
						{
							token' = token + 1;
							a' = applyEdit edit a;
						}
						in do
						{
							forM (allStore store) (\u -> u token' edit);
							return ((store,a',token'),Just (Just ()));
						}
						else return ((store,a,token),Nothing);
					}),
					subClose = modifyMVar_ stateVar (\(store,a,token) -> return (deleteStore key store,a,token))
				});
			}
		};
	};

	lensObject :: (Eq state) => StateLens state a b -> (a -> IO state) -> Object context a -> Object context b;
	lensObject lens getstate (MkObject context subscribe) = MkObject
	{
		objContext = context,
		objSubscribe = \initialise updater -> do
		{
			((var,r),token,sub) <- subscribe (\a -> do
			{
				state <- getstate a;
				r <- initialise (slensGet lens state a);
				var <- newTVarIO (a,state);
				return (var,r);
			}) (\(var,r) token edita -> do
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
					Just editb -> updater r token editb;
					_ -> return ();
				}
			});
			return (r,token,MkSubscription
			{
				subPush = \token' edit -> do
				{
					(_,state) <- atomically (readTVar var);
					subPush sub token' (StateLensEdit lens state edit);
				},
				subClose = subClose sub
			});
		}
	};
}
