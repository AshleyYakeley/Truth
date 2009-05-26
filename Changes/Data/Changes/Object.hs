module Data.Changes.Object where
{
	import Data.Changes.Edit;
	import Data.Store;
	import Control.Concurrent.MVar;
	import Control.Exception hiding (catch);
	import Data.ConstFunction;
	import Data.Traversable;

	-- | change the object.
	-- The action argument and subscription updates are mutually excluded.
	-- Returns Nothing if change is not allowed.
	-- Returns Just () if change succeeded, and updates will be received before this action returns.
	;
	type Push a = IO (Maybe (Edit a)) -> IO (Maybe ());

	data Subscription a = MkSubscription
	{
		subObject :: Object a,
		
		-- | close the subscription
		subClose :: IO ()
	};
	
	data Object a = MkObject
	{
		-- | blocks if the object is busy
		objSubscribe :: forall r. (a -> Push a -> IO r) -> (r -> Edit a -> IO ()) -> IO (r, Subscription a)
	};
	
	data Editor a b = forall r. MkEditor
	{
		editorInit :: a -> Push a -> IO r,
		editorUpdate :: r -> Edit a -> IO (),
		editorDo :: r -> Object a -> IO b
	};

	withSubscription :: Object a -> Editor a b -> IO b;
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
	
	readObject :: Object a -> IO a;
	readObject object = withSubscription object (MkEditor
	{
		editorInit = \a _ -> return a,
		editorUpdate = \_ _ -> return (),
		editorDo = \a _ -> return a
	});
{-
	writeObject :: a -> Object a -> IO (Maybe ());
	writeObject a object = withSubscription object (MkEditor
	{
		editorInit = \_ push -> return push,
		editorUpdate = \_ _ -> return (),
		editorDo = \push _ -> push (return (ReplaceEdit a))
	});
-}
	data InternalObject a = MkInternalObject
	{
		intobjGetInitial :: forall r. (a -> IO r) -> IO r,
		intobjPush :: Push a,
		intobjClose :: IO ()
	};

	makeObject :: forall a. ((Edit a -> IO ()) -> IO (InternalObject a)) -> Object a;
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
					IO (r, Subscription a);
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
	
	makeFreeObject :: forall a. (Editable a) => a -> Object a;
	makeFreeObject initial = makeObject (\pushOut -> do
	{
		statevar <- newMVar initial;
		return (MkInternalObject
		{
			intobjGetInitial = withMVar statevar,
			intobjPush = \ioedit -> modifyMVar statevar (\a -> do
			{
				medit <- ioedit;
				case medit of
				{
					Just edit -> do
					{
						pushOut edit;
						return (applyEdit edit a,Just ());
					};
					Nothing -> return (a,Nothing);
				};
			}),
			intobjClose = return ()
		});
	});

	lensObject :: forall state a b. (Editable a,Eq state) => FloatingLens state a b -> state -> Object a -> Object b;
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
			(newstate,meditb) = applyConstFunction (lensUpdate lens edita oldstate) olda;
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
			intobjPush = \ioedit -> push (withMVar statevar (\(state,a) -> do
			{
				medit <- ioedit;
				return (do
				{
					edit <- medit;
					applyConstFunction (lensPutEdit lens state edit) a;
				});
			})),
			intobjClose = subClose sub
		});
	});
}
