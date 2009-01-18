module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.STM;
	import Control.Concurrent.MVar;
	import Control.Exception hiding (catch);
	import Data.Traversable;
	
	data Subscription token a = MkSubscription
	{
		-- | change the object.
		-- returns Nothing if not synchronised.
		-- returns Just Nothing if change is not allowed
		-- returns Just (Just ()) if change succeeded, and updates will be received before this action returns
		subPush :: token -> Edit a -> IO (Maybe (Maybe ())),
		
		-- | close the subscription
		subClose :: IO ()
	};
	
	data Object context a = forall token. MkObject
	{
		objContext :: context,
		
		-- | returns Nothing if the object is busy
		objSubscribe :: forall r. (a -> IO r) -> (r -> token -> Edit a -> IO ()) -> IO (Maybe (r, token, Subscription token a))
	};
	
	data Editor_ token a b = forall r. MkEditor_
	{
		editorInit :: a -> IO r,
		editorUpdate :: r -> token -> Edit a -> IO (),
		editorDo :: r -> token -> (token -> Edit a -> IO (Maybe (Maybe ()))) -> IO (Maybe b)
	};
	
	type Editor a b = forall token. Editor_ token a b;
	
	withSubscription :: Object context a -> Editor a b -> IO (Maybe b);
	withSubscription (MkObject _ subscribe) editor = case editor of 
	{
		(MkEditor_ initr update f) -> do
		{
			mstuff <- subscribe initr update;
			case mstuff of
			{
				Just (r, token, sub) -> finally
					(f r token (subPush sub))
					(subClose sub);
				_ -> return Nothing;
			};
		};
	};
	
	readObject :: Object context a -> IO (Maybe a);
	readObject object = withSubscription object (MkEditor_
	{
		editorInit = return,
		editorUpdate = \_ _ _ -> return (),
		editorDo = \a _ _ -> return (Just a)
	});

	writeObject :: a -> Object context a -> IO (Maybe (Maybe ()));
	writeObject a object = withSubscription object (MkEditor_
	{
		editorInit = return,
		editorUpdate = \_ _ _ -> return (),
		editorDo = \_ token push -> push token (ReplaceEdit a)
	});

	makeFreeObject :: context -> a -> IO (Object context a);
	makeFreeObject context initial = do
	{
		stateVar <- newMVar (emptyStore,initial,0 :: Integer);
		return MkObject
		{
			objContext = context,

			objSubscribe = \initialise updater -> do
			{
				(r,key,firsttoken) <- modifyMVar stateVar (\(store,a,token) -> do
				{
					r <- initialise a;
					let {(key,newstore) = addStore (updater r) store;};
					return ((newstore,a,token),(r,key,token));
				});
				return (Just (r,firsttoken,MkSubscription
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
				}));
			}
		};
	};

	lensObject :: (Eq state) => StateLens state a b -> (a -> IO state) -> Object context a -> Object context b;
	lensObject lens getstate (MkObject context subscribe) = MkObject
	{
		objContext = context,
		objSubscribe = \initialise updater -> do
		{
			mstuff <- subscribe (\a -> do
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
			case mstuff of
			{
				Just ((var,r),token,sub) -> return (Just (r,token,MkSubscription
				{
					subPush = \token' edit -> do
					{
						(_,state) <- atomically (readTVar var);
						subPush sub token' (StateLensEdit lens state edit);
					},
					subClose = subClose sub
				}));
				_ -> return Nothing;
			};
		}
	};
}
