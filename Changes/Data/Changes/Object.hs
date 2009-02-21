module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.MVar;
	import Control.Exception hiding (catch);
	import Data.Traversable;

	data Subscription context token a = MkSubscription
	{
		subGetContext :: IO context,

		subObject :: Object context a,

		-- | change the object.
		-- returns Nothing if not synchronised.
		-- returns Just Nothing if change is not allowed
		-- returns Just (Just ()) if change succeeded, and updates will be received before this action returns
		subPush :: token -> Edit a -> IO (Maybe (Maybe ())),

--		-- | with object lock
--		subWithLock :: forall b. (a -> (Edit a -> IO (Maybe ())) -> IO b) -> IO b,
		
		-- | close the subscription
		subClose :: IO ()
	};
	
	data Object context a = forall token. (Eq token) => MkObject
	{
		-- | blocks if the object is busy
		objSubscribe :: forall r. (a -> IO r) -> (r -> token -> Maybe (Edit a) -> IO ()) -> IO (r, token, Subscription context token a)
	};
	
	data Editor_ token context a b = forall r. MkEditor_
	{
		editorInit :: a -> IO r,
		editorUpdate :: r -> token -> Maybe (Edit a) -> IO (),
		editorDo :: r -> token -> Object context a -> (token -> Edit a -> IO (Maybe (Maybe ()))) -> IO b
	};
	
	type Editor context a b = forall token. (Eq token) => Editor_ token context a b;
	
	withSubscription :: Object context a -> Editor context a b -> IO b;
	withSubscription (MkObject subscribe) editor = case editor of 
	{
		(MkEditor_ initr update f) -> do
		{
			(r, token, sub) <- subscribe initr update;
			finally
				(f r token (subObject sub) (subPush sub))
				(subClose sub);
		};
	};
	
	readObject :: Object context a -> IO a;
	readObject object = withSubscription object (MkEditor_
	{
		editorInit = return,
		editorUpdate = \_ _ _ -> return (),
		editorDo = \a _ _ _ -> return a
	});

	writeObject :: a -> Object context a -> IO (Maybe (Maybe ()));
	writeObject a object = withSubscription object (MkEditor_
	{
		editorInit = return,
		editorUpdate = \_ _ _ -> return (),
		editorDo = \_ token _ push -> push token (ReplaceEdit a)
	});

	newtype IntegerToken = MkIntegerToken Integer deriving (Eq);
	
	firstToken :: IntegerToken;
	firstToken = MkIntegerToken 0;
	
	nextToken :: IntegerToken -> IntegerToken;
	nextToken (MkIntegerToken i) = MkIntegerToken (i + 1);

	pushStore :: forall token a. Store (token -> Maybe (Edit a) -> IO ()) -> token -> Maybe (Edit a) -> IO ();
	pushStore store token medit = forM (allStore store) (\u -> u token medit) >> return ();
	
	type StoreVar token a = MVar (Store (token -> Maybe (Edit a) -> IO ()));

	newStoreVar :: IO (StoreVar token a);
	newStoreVar = newMVar emptyStore;

	closeStoreVar :: StoreVar token a -> Int -> IO () -> IO ();
	closeStoreVar var key doclose = modifyMVar_ var (\store -> let
	{
		newstore = deleteStore key store;
	} in do
	{
		if isEmptyStore newstore
		 then doclose
		 else return ();
		return newstore;
	});
	
	addStoreVar :: StoreVar token a -> (token -> Maybe (Edit a) -> IO ()) -> IO Int;
	addStoreVar storevar update = modifyMVar storevar (\store -> do
	{
		let {(key,newstore) = addStore update store;};
		return (newstore,key);
	});
	
	pushStoreVar :: StoreVar token a -> token -> Maybe (Edit a) -> IO ();
	pushStoreVar storevar token medit = withMVar storevar (\store -> pushStore store token medit);
	
	makeFreeObject :: forall context a. context -> a -> Object context a;
	makeFreeObject context initial = let
	{
		objsub :: forall r. 
			StoreVar IntegerToken a ->
			MVar (a, IntegerToken) ->
			(a -> IO r) -> 
			(r -> IntegerToken -> Maybe (Edit a) -> IO ()) -> 
			IO (r, IntegerToken, Subscription context IntegerToken a);
		objsub storevar statevar initialise updater = do
		{
			(r,key,firsttoken) <- withMVar statevar (\(a,token) -> do
			{
				r <- initialise a;
				key <- addStoreVar storevar (updater r);
				return (r,key,token);
			});
			return (r,firsttoken,MkSubscription
			{
				subObject = MkObject
				{
					objSubscribe = objsub storevar statevar
				},
				subGetContext = return context,
				subPush = \pushtoken edit -> modifyMVar statevar (\(a,token) -> do
				{
					if pushtoken == token then let
					{
						token' = nextToken token;
						a' = applyEdit edit a;
					}
					in do
					{
						pushStoreVar storevar token' (Just edit);
						return ((a',token'),Just (Just ()));
					}
					else return ((a,token),Nothing);
				}),
				subClose = closeStoreVar storevar key (return ())
			});
		};
	}
	in MkObject
	{
		objSubscribe = \initialise updater -> do
		{
			storevar <- newStoreVar;
			stateVar <- newMVar (initial,firstToken);
			objsub storevar stateVar initialise updater
		}
	};

	lensObject :: forall context state a b. (Eq state) => FloatingLens state a b -> state -> Object context a -> Object context b;
	lensObject lens firststate (MkObject subscribe) = let
	{
		makeLensObject pushOut = do
		{
			((statevar,firsta),firsttoken,sub) <- subscribe 
			 (\a -> do
			{
				statevar <- newEmptyMVar;
				return (statevar,a);
			}) 
			 (\(statevar,_) token medita -> modifyMVar_ statevar (\(oldstate,_,olda) -> let
			{
				(newa,(newstate,meditb)) = case medita of
				{
					Just edita -> (applyEdit edita olda,lensUpdate lens olda edita oldstate);
					_ -> (olda,(oldstate,Nothing));
				};
			} in do
			{
				pushOut token meditb;
				return (newstate,token,newa);
			}));
			putMVar statevar (firststate,firsttoken,firsta);
			return (statevar,sub);
		};
	
		objsub :: forall r token. (Eq token) => 
			StoreVar token b ->
			MVar (state, token, a) ->
			Subscription context token a ->
			(b -> IO r) -> 
			(r -> token -> Maybe (Edit b) -> IO ()) -> 
			IO (r, token, Subscription context token b);
		objsub storevar statevar sub initialise updater = do
		{
			(r,key,firsttoken) <- withMVar statevar (\(state,token,a) -> do
			{
				r <- initialise (lensGet lens state a);		
				key <- addStoreVar storevar (updater r);
				return (r,key,token);
			});		
			return (r,firsttoken,MkSubscription
			{
				subObject = MkObject
				{
					objSubscribe = objsub storevar statevar sub
				},
				subGetContext = subGetContext sub,
				subPush = \pushtoken edit -> do
				{
					mstate <- withMVar statevar (\(state,curtoken,_) -> return
					 (if pushtoken == curtoken then Just state else Nothing));
					case mstate of
					{
						Just state -> subPush sub pushtoken (StateLensEdit lens state edit);
						_ -> return Nothing;
					};
				},
				subClose = closeStoreVar storevar key (subClose sub)
			});
		}
	}
	in MkObject
	{
		objSubscribe = \initialise updater -> do
		{
			storevar <- newStoreVar;
			(statevar,sub) <- makeLensObject (pushStoreVar storevar);
			objsub storevar statevar sub initialise updater;
		}
	};
}
