module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.MVar;
	import Control.Exception hiding (catch);
	import Data.Traversable;

	data Subscription context a = MkSubscription
	{
		subGetContext :: IO context,

		subObject :: Object context a,

		-- | change the object.
		-- the action argument and subscription updates are mutually excluded
		-- returns Nothing if not synchronised (i.e. further subscription updates to run)
		-- returns Just Nothing if change is not allowed
		-- returns Just (Just ()) if change succeeded, and updates will be received before this action returns
		subPush :: IO (Edit a) -> IO (Maybe (Maybe ())),
		
		-- | close the subscription
		subClose :: IO ()
	};
	
	data Object context a = MkObject
	{
		-- | blocks if the object is busy
		objSubscribe :: forall r. (a -> IO r) -> (r -> Maybe (Edit a) -> IO ()) -> IO (r, Subscription context a)
	};
	
	data Editor context a b = forall r. MkEditor
	{
		editorInit :: a -> IO r,
		editorUpdate :: r -> Maybe (Edit a) -> IO (),
		editorDo :: r -> Object context a -> (IO (Edit a) -> IO (Maybe (Maybe ()))) -> IO b
	};

	withSubscription :: Object context a -> Editor context a b -> IO b;
	withSubscription (MkObject subscribe) editor = case editor of 
	{
		(MkEditor initr update f) -> do
		{
			(r, sub) <- subscribe initr update;
			finally
				(f r (subObject sub) (subPush sub))
				(subClose sub);
		};
	};
	
	readObject :: Object context a -> IO a;
	readObject object = withSubscription object (MkEditor
	{
		editorInit = return,
		editorUpdate = \_ _ -> return (),
		editorDo = \a _ _ -> return a
	});

	writeObject :: a -> Object context a -> IO (Maybe (Maybe ()));
	writeObject a object = withSubscription object (MkEditor
	{
		editorInit = return,
		editorUpdate = \_ _ -> return (),
		editorDo = \_ _ push -> push (return (ReplaceEdit a))
	});

	pushStore :: forall a. Store (Maybe (Edit a) -> IO ()) -> Maybe (Edit a) -> IO ();
	pushStore store medit = forM (allStore store) (\u -> u medit) >> return ();
	
	type StoreVar a = MVar (Store (Maybe (Edit a) -> IO ()));

	newStoreVar :: IO (StoreVar a);
	newStoreVar = newMVar emptyStore;

	closeStoreVar :: StoreVar a -> Int -> IO () -> IO ();
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
	
	addStoreVar :: StoreVar a -> (Maybe (Edit a) -> IO ()) -> IO Int;
	addStoreVar storevar update = modifyMVar storevar (\store -> do
	{
		let {(key,newstore) = addStore update store;};
		return (newstore,key);
	});
	
	pushStoreVar :: StoreVar a -> Maybe (Edit a) -> IO ();
	pushStoreVar storevar medit = withMVar storevar (\store -> pushStore store medit);
	
	makeFreeObject :: forall context a. context -> a -> Object context a;
	makeFreeObject context initial = let
	{
		objsub :: forall r. 
			StoreVar a ->
			MVar a ->
			(a -> IO r) -> 
			(r -> Maybe (Edit a) -> IO ()) -> 
			IO (r, Subscription context a);
		objsub storevar statevar initialise updater = do
		{
			(r,key) <- withMVar statevar (\a -> do
			{
				r <- initialise a;
				key <- addStoreVar storevar (updater r);
				return (r,key);
			});
			return (r,MkSubscription
			{
				subObject = MkObject
				{
					objSubscribe = objsub storevar statevar
				},
				subGetContext = return context,
				subPush = \ioedit -> modifyMVar statevar (\a -> do
				{
					edit <- ioedit;
					pushStoreVar storevar (Just edit);
					return (applyEdit edit a,Just (Just ()));
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
			stateVar <- newMVar initial;
			objsub storevar stateVar initialise updater
		}
	};

	lensObject :: forall context state a b. (Eq state) => FloatingLens state a b -> state -> Object context a -> Object context b;
	lensObject lens firststate (MkObject subscribe) = let
	{
		makeLensObject pushOut = do
		{
			((statevar,firsta),sub) <- subscribe 
			 (\a -> do
			{
				statevar <- newEmptyMVar;
				return (statevar,a);
			}) 
			 (\(statevar,_) medita -> modifyMVar_ statevar (\(oldstate,olda) -> let
			{
				(newa,(newstate,meditb)) = case medita of
				{
					Just edita -> (applyEdit edita olda,lensUpdate lens olda edita oldstate);
					_ -> (olda,(oldstate,Nothing));
				};
			} in do
			{
				pushOut meditb;
				return (newstate,newa);
			}));
			putMVar statevar (firststate,firsta);
			return (statevar,sub);
		};
	
		objsub :: 
			StoreVar b ->
			MVar (state, a) ->
			Subscription context a ->
			(b -> IO r) -> 
			(r -> Maybe (Edit b) -> IO ()) -> 
			IO (r, Subscription context b);
		objsub storevar statevar sub initialise updater = do
		{
			(r,key) <- withMVar statevar (\(state,a) -> do
			{
				r <- initialise (lensGet lens state a);		
				key <- addStoreVar storevar (updater r);
				return (r,key);
			});		
			return (r,MkSubscription
			{
				subObject = MkObject
				{
					objSubscribe = objsub storevar statevar sub
				},
				subGetContext = subGetContext sub,
				subPush = \ioedit -> subPush sub (withMVar statevar (\(state,_) -> do
				{
					edit <- ioedit;
					return (StateLensEdit lens state edit);
				})),
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
