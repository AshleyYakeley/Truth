module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.MVar;
	import Control.Exception hiding (catch);
	import Data.Traversable;

	-- | change the object.
	-- The action argument and subscription updates are mutually excluded.
	-- Returns Nothing if change is not allowed.
	-- Returns Just () if change succeeded, and updates will be received before this action returns.
	;
	type Push a = IO (Edit a) -> IO (Maybe ());

	data Subscription context a = MkSubscription
	{
		subGetContext :: IO context,

		subObject :: Object context a,
		
		-- | close the subscription
		subClose :: IO ()
	};
	
	data Object context a = MkObject
	{
		-- | blocks if the object is busy
		objSubscribe :: forall r. (a -> Push a -> IO r) -> (r -> Edit a -> IO ()) -> IO (r, Subscription context a)
	};
	
	data Editor context a b = forall r. MkEditor
	{
		editorInit :: a -> Push a -> IO r,
		editorUpdate :: r -> Edit a -> IO (),
		editorDo :: r -> Object context a -> IO b
	};

	withSubscription :: Object context a -> Editor context a b -> IO b;
	withSubscription (MkObject subscribe) editor = case editor of 
	{
		(MkEditor initr update f) -> do
		{
			(r, sub) <- subscribe initr update;
			finally
				(f r (subObject sub))
				(subClose sub);
		};
	};
	
	readObject :: Object context a -> IO a;
	readObject object = withSubscription object (MkEditor
	{
		editorInit = \a _ -> return a,
		editorUpdate = \_ _ -> return (),
		editorDo = \a _ -> return a
	});

	writeObject :: a -> Object context a -> IO (Maybe ());
	writeObject a object = withSubscription object (MkEditor
	{
		editorInit = \_ push -> return push,
		editorUpdate = \_ _ -> return (),
		editorDo = \push _ -> push (return (ReplaceEdit a))
	});

	data InternalObject context a = MkInternalObject
	{
		intobjGetInitial :: forall r. (a -> IO r) -> IO r,
		intobjGetContext :: IO context,
		intobjPush :: Push a,
		intobjClose :: IO ()
	};

	makeObject :: forall context a. ((Edit a -> IO ()) -> IO (InternalObject context a)) -> Object context a;
	makeObject getIntObject = MkObject
	{
		objSubscribe = \initialise' updater' -> do
		{
			storevar <- newMVar emptyStore;
			intobj <- getIntObject (\edit -> withMVar storevar (\store -> forM (allStore store) (\u -> u edit) >> return ()));
			let
			{
				objSub :: forall r.
					(a -> Push a -> IO r) -> 
					(r -> Edit a -> IO ()) -> 
					IO (r, Subscription context a);
				objSub initialise updater = do
				{
					r <- intobjGetInitial intobj (\a -> initialise a (intobjPush intobj));
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
				pushOut edit;
				return (applyEdit edit a,Just ());
			}),
			intobjClose = return ()
		});
	});

	lensObject :: forall context state a b. (Eq state) => FloatingLens state a b -> state -> Object context a -> Object context b;
	lensObject lens firststate (MkObject subscribe) = makeObject (\pushOut -> do
	{
		((statevar,firsta,push),sub) <- subscribe 
		 (\a push -> do
		{
			statevar <- newEmptyMVar;
			return (statevar,a,push);
		}) 
		 (\(statevar,_,_) edita -> modifyMVar_ statevar (\(oldstate,olda) -> let
		{
			(newstate,meditb) = lensUpdate lens olda edita oldstate;
		} in do
		{
			case meditb of
			{
				Just editb -> pushOut editb;
				_ -> return ();
			};
			return (newstate,applyEdit edita olda);
		}));
		putMVar statevar (firststate,firsta);
		return (MkInternalObject
		{
			intobjGetInitial = \initialise -> withMVar statevar (\(state,a) -> initialise (lensGet lens state a)),
			intobjGetContext = subGetContext sub,
			intobjPush = \ioedit -> push (withMVar statevar (\(state,_) -> do
			{
				edit <- ioedit;
				return (StateLensEdit lens state edit);
			})),
			intobjClose = subClose sub
		});
	});
}
