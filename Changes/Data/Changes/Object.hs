module Data.Changes.Object where
{
	import Data.Changes.FloatingLens;
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
		subCopy :: Subscribe a,
		
		-- | close the subscription
		subClose :: IO ()
	};
	
	-- | blocks if the object is busy
	;
	type Subscribe a = forall r. (a -> Push a -> IO r) -> (r -> Edit a -> IO ()) -> IO (r, Subscription a);
	
	data Editor a b = forall r. MkEditor
	{
		editorInit :: a -> Push a -> IO r,
		editorUpdate :: r -> Edit a -> IO (),
		editorDo :: r -> Subscribe a -> IO b
	};

	subscribeEdit :: Subscribe a -> Editor a b -> IO b;
	subscribeEdit subscribe editor = case editor of 
	{
		(MkEditor initr update f) -> do
		{
			(r, sub) <- subscribe initr update;
			finally
				(f r (subCopy sub))
				(subClose sub);
		};
	};
	
	subscribeRead :: Subscribe a -> IO a;
	subscribeRead object = subscribeEdit object (MkEditor
	{
		editorInit = \a _ -> return a,
		editorUpdate = \_ _ -> return (),
		editorDo = \a _ -> return a
	});

	subscribeWrite :: a -> Subscribe a -> IO (Maybe ());
	subscribeWrite a object = subscribeEdit object (MkEditor
	{
		editorInit = \_ push -> return push,
		editorUpdate = \_ _ -> return (),
		editorDo = \push _ -> push (return (Just (ReplaceEdit a)))
	});

	data Object a = MkObject
	{
		objGetInitial :: forall r. (a -> IO r) -> IO r,
		objPush :: Push a,
		objClose :: IO ()
	};

	objSubscribe :: forall a. ((Edit a -> IO ()) -> IO (Object a)) -> Subscribe a;
	objSubscribe getIntObject initialise' updater' = do
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
				r <- objGetInitial intobj (\a -> initialise a (objPush intobj));
				key <- modifyMVar storevar (\store -> do
				{
					let {(key,newstore) = addStore (updater r) store;};
					return (newstore,key);
				});
				return (r,MkSubscription
				{
					subCopy = objSub,
					subClose = modifyMVar_ storevar (\store -> let
					{
						newstore = deleteStore key store;
					} in do
					{
						if isEmptyStore newstore
						 then (objClose intobj)
						 else return ();
						return newstore;
					})
				});
			};
		};
		objSub initialise' updater';
	};
	
	freeObjSubscribe :: forall a. (Editable a) => a -> Subscribe a;
	freeObjSubscribe initial = objSubscribe (\pushOut -> do
	{
		statevar <- newMVar initial;
		return (MkObject
		{
			objGetInitial = withMVar statevar,
			objPush = \ioedit -> modifyMVar statevar (\a -> do
			{
				medit <- ioedit;
				case medit of
				{
					Just edit -> do
					{
						pushOut edit;
						return (applyConstFunction (applyEdit edit) a,Just ());
					};
					Nothing -> return (a,Nothing);
				};
			}),
			objClose = return ()
		});
	});

	lensSubscribe :: forall state a b. (Editable a,Eq state) => FloatingLens state a b -> state -> Subscribe a -> Subscribe b;
	lensSubscribe lens firststate subscribe = objSubscribe (\pushOut -> do
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
			return (newstate,applyConstFunction (applyEdit edita) olda);
		}));
		putMVar statevar (firststate,firsta);
		return (MkObject
		{
			objGetInitial = \initialise -> withMVar statevar (\(state,a) -> initialise (lensGet lens state a)),
			objPush = \ioedit -> push (withMVar statevar (\(state,a) -> do
			{
				medit <- ioedit;
				return (do
				{
					edit <- medit;
					applyConstFunction (lensPutEdit lens state edit) a;
				});
			})),
			objClose = subClose sub
		});
	});
}
