module Object where
{
	import Edit;
	import Lens;
	import System.Gnome.VFS;
	import Control.Concurrent.STM;
	import Control.Concurrent;
	
	data Subscription a = MkSubscription
	{
		subInitial :: a,
		subPull :: IO (Maybe (Edit a)),
		subPush :: [Edit a] -> IO (Maybe [Edit a]),
		subClose :: IO ()
	};
	
	data Object a = MkObject
	{
		objContext :: URI,
		subscribe :: IO (Subscription a)
	};
	
	withSubscription :: Object a -> (a -> IO (Maybe (Edit a)) -> ([Edit a] -> IO (Maybe [Edit a])) -> IO b) -> IO b;
	withSubscription obj foo = do
	{
		sub <- subscribe obj;
		b <- foo (subInitial sub) (subPull sub) (subPush sub);
		subClose sub;
		return b;
	};
	
	readObject :: Object a -> IO a;
	readObject obj = withSubscription obj (\state _ _ -> return state);
	
	pump :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ();
	pump pull push = pp where
	{
		pp = do
		{
			ma <- pull;
			case ma of
			{
				Just a -> do
				{
					push a;
					pp;
				};
				Nothing -> return ();
			};
		};
	};
	
	writeObject :: a -> Object a -> IO ();
	writeObject a obj = withSubscription obj (\_ _ pushEdit -> pump (pushEdit [ReplaceEdit a]) (\_ -> return ()));
	
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
	
	pumpThread :: IO (Maybe a) -> (a -> IO ()) -> IO ThreadId;
	pumpThread pull push = forkIO (pump pull push);
	
	makeSubscribable :: forall a. a -> STM (STM (a,STM (Maybe (Edit a)),[Edit a] -> STM (Maybe [Edit a]),STM ()));
	makeSubscribable initial = do
	{
		stateVar <- newTVar initial;
		queuesVar <- newTVar Nothing;
		return (do
		{
			state <- readTVar stateVar;
			mchan <- readTVar queuesVar;
			thisQueue <- case mchan of
			{
				Nothing -> do
				{
					thisQueue <- newTChan;
					writeTVar queuesVar (Just thisQueue);
					return thisQueue;
				};
				Just chan -> dupTChan chan;
			};
			
			isOpenVar <- newTVar True;
			
			let
			{
				pullEdit :: STM (Edit a);
				pullEdit = readTChan thisQueue;
				
				pullEdits :: STM [Edit a];
				pullEdits = do
				{
					edits <- readListTChan thisQueue;
					case edits of
					{
						[] -> retry;
						_ -> return edits;
					};
				};
				
				pushEdit :: [Edit a] -> STM (Maybe [Edit a]);
				pushEdit edits = orElse pullPendingEdits pushEditOK where
				{
					pullPendingEdits = do
					{
						pendingEdits <- pullEdits;
						return (Just pendingEdits);
					};
					
					pushEditOK = do
					{
						-- update state
						modifyTVar stateVar (applyEdits edits);
						
						-- push out edit to queues
						writeListTChan thisQueue edits;
						
						-- remove from thisQueue (this will be the only item)
						pullEdit;
						
						return Nothing;
					}
				};
				
				pullMaybeEdit :: STM (Maybe (Edit a));
				pullMaybeEdit = do
				{
					isOpen <- readTVar isOpenVar;
					if isOpen then do
					{
						edit <- pullEdit;
						return (Just edit);
					} else return Nothing;
				};
				
				closer :: STM ();
				closer = writeTVar isOpenVar False;
			};
			return (state,pullMaybeEdit,pushEdit,closer);
		});
	};

	makeFreeObject :: forall a. URI -> a -> IO (Object a);
	makeFreeObject context initial = atomically (do
	{
		subscribable <- makeSubscribable initial;
		return MkObject
		{
			objContext = context,

			subscribe = atomically (do
			{
				(state,pullEdit,pushEdit,closer) <- subscribable;
				return (MkSubscription state (atomically pullEdit) (atomically . pushEdit) (atomically closer));
			})
		};
	});
	
	{-
	subscriptionObject :: Object a -> IO (Object a);
	subscriptionObject obj = do
	{
		(state,pullEdit,pushEdit,closer) <- subscribe obj;
		return (MkObject
		{
			context = context obj
		});
	};
	-}
	
	
--	unsavedObject :: Object a -> IO (Object a,)
--	{
		
--	};
	
	type AnyObject = AnyV Object;
	
	{-
	collectObjects :: URI -> [AnyObject] -> AnyObject;
	collectObjects [] = MkAnyF (CollectionValueType NilListType) ();
	collectObjects _ = undefined;
	
	oneOrCollectObjects :: [AnyObject] -> Maybe AnyObject;
	oneOrCollectObjects [] = Nothing;
	oneOrCollectObjects [obj] = Just obj;
	oneOrCollectObjects objs = Just (collectObjects objs);
	-}
	
	lensObject :: forall a b. LensC a b -> Object a -> Object b;
	lensObject lens obj = MkObject
	{
		objContext = objContext obj,
		subscribe = do
		{
			let
			{
				processEdits :: [Edit a] -> [Edit b];
				processEdits = fmap processEdit;
				
				processEdit :: Edit a -> Edit b;
				processEdit (ReplaceEdit a) = ReplaceEdit (lensGet lens a);
				processEdit _ = undefined; -- NYI
			};
			sub <- subscribe obj;
			let {initial = subInitial sub;};
			--var <- newTVarIO initial;
			return (MkSubscription
			{
				subInitial = lensGet lens initial,
				subPull = do
				{
					--olda <- readTVar var;
					medit <- subPull sub;
					return (fmap processEdit medit);
				},
				subPush = \edits -> do
				{
					mes <- subPush sub (fmap (LensEdit lens) edits);
					case mes of
					{
						Nothing -> return Nothing;
						Just pendedits -> return (Just (processEdits pendedits));
					};
				},
				subClose = subClose sub
			});
		}
	};
	
}
