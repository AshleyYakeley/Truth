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

	data InternalObject context a = MkInternalObject
	{
		intobjGetInitial :: forall r. (a -> IO r) -> IO r,
		intobjGetContext :: IO context,
		intobjPush :: IO (Edit a) -> IO (Maybe (Maybe ())),
		intobjClose :: IO ()
	};

	makeObject :: forall context a. ((Maybe (Edit a) -> IO ()) -> IO (InternalObject context a)) -> Object context a;
	makeObject getIntObject = MkObject
	{
		objSubscribe = \initialise' updater' -> do
		{
			storevar <- newMVar emptyStore;
			intobj <- getIntObject (\medit -> withMVar storevar (\store -> forM (allStore store) (\u -> u medit) >> return ()));
			let
			{
				objSub :: forall r.
					(a -> IO r) -> 
					(r -> Maybe (Edit a) -> IO ()) -> 
					IO (r, Subscription context a);
				objSub initialise updater = do
				{
					r <- intobjGetInitial intobj initialise;
					key <- modifyMVar storevar (\store -> do
					{
						let {(key,newstore) = addStore (updater r) store;};
						return (newstore,key);
					});
					return (r,MkSubscription
					{
						subObject = MkObject
						{
							objSubscribe = objSub
						},
						subGetContext = intobjGetContext intobj,
						subPush = intobjPush intobj,
						subClose = modifyMVar_ storevar (\store -> let
						{
							newstore = deleteStore key store;
						} in do
						{
							if isEmptyStore newstore
							 then (intobjClose intobj)
							 else return ();
							return newstore;
						})
					});
				};
			};
			objSub initialise' updater';
		}
	};
	
	makeFreeObject :: forall context a. context -> a -> Object context a;
	makeFreeObject context initial = makeObject (\pushOut -> do
	{
		statevar <- newMVar initial;
		return (MkInternalObject
		{
			intobjGetInitial = withMVar statevar,
			intobjGetContext = return context,
			intobjPush = \ioedit -> modifyMVar statevar (\a -> do
			{
				edit <- ioedit;
				pushOut (Just edit);
				return (applyEdit edit a,Just (Just ()));
			}),
			intobjClose = return ()
		});
	});

	lensObject :: forall context state a b. (Eq state) => FloatingLens state a b -> state -> Object context a -> Object context b;
	lensObject lens firststate (MkObject subscribe) = makeObject (\pushOut -> do
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
		return (MkInternalObject
		{
			intobjGetInitial = \initialise -> withMVar statevar (\(state,a) -> initialise (lensGet lens state a)),
			intobjGetContext = subGetContext sub,
			intobjPush = \ioedit -> subPush sub (withMVar statevar (\(state,_) -> do
			{
				edit <- ioedit;
				return (StateLensEdit lens state edit);
			})),
			intobjClose = subClose sub
		});
	});
}
