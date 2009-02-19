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
	
	makeFreeObject :: forall context a. context -> a -> Object context a;
	makeFreeObject context initial = let
	{
		objsub :: forall r. MVar (Store (IntegerToken -> Maybe (Edit a) -> IO ()), a, IntegerToken) -> (a -> IO r) -> (r -> IntegerToken -> Maybe (Edit a) -> IO ()) -> IO (r, IntegerToken, Subscription context IntegerToken a);
		objsub svar = \initialise updater -> do
		{
			(r,key,firsttoken) <- modifyMVar svar (\(store,a,token) -> do
			{
				r <- initialise a;
				let {(key,newstore) = addStore (updater r) store;};
				return ((newstore,a,token),(r,key,token));
			});
			return (r,firsttoken,MkSubscription
			{
				subObject = MkObject
				{
					objSubscribe = objsub svar
				},
				subGetContext = return context,
				subPush = \edittoken edit -> modifyMVar svar (\(store,a,token) -> do
				{
					if edittoken == token then let
					{
						token' = nextToken token;
						a' = applyEdit edit a;
					}
					in do
					{
						pushStore store token' (Just edit);
						return ((store,a',token'),Just (Just ()));
					}
					else return ((store,a,token),Nothing);
				}),
				subClose = modifyMVar_ svar (\(store,a,token) -> return (deleteStore key store,a,token))
			});
		};
	}
	in MkObject
	{
		objSubscribe = \initialise updater -> do
		{
			stateVar <- newMVar (emptyStore,initial,firstToken);
			objsub stateVar initialise updater
		}
	};

	lensObject :: forall context state a b. (Eq state) => FloatingLens state a b -> state -> Object context a -> Object context b;
	lensObject lens firststate (MkObject subscribe) = let
	{
		objsub :: forall r token. (Eq token) => 
			MVar (Store (token -> Maybe (Edit b) -> IO ()), state, token, a) ->
			Subscription context token a ->
			(b -> IO r) -> (r -> token -> Maybe (Edit b) -> IO ()) -> IO (r, token, Subscription context token b);
		objsub var sub = \initialise updater -> do
		{
			(r,firsttoken,key) <- modifyMVar var (\(store,state,token,a) -> do
			{
				r <- initialise (lensGet lens state a);			
				let {(key,newstore) = addStore (updater r) store;};
				return ((newstore,state,token,a),(r,token,key));
			});
			
			return (r,firsttoken,MkSubscription
			{
				subObject = MkObject
				{
					objSubscribe = objsub var sub
				},
				subGetContext = subGetContext sub,
				subPush = \pushtoken edit -> do
				{
					mstate <- withMVar var (\(_,state,curtoken,_) -> return
					 (if pushtoken == curtoken then Just state else Nothing));
					case mstate of
					{
						Just state -> subPush sub pushtoken (StateLensEdit lens state edit);
						_ -> return Nothing;
					};
				},
				subClose = modifyMVar_ var (\(store,state,token',a) -> let
				{
					newstore = deleteStore key store;
				} in do
				{
					if isEmptyStore newstore
					 then subClose sub
					 else return ();
					return (newstore,state,token',a);
				})
			});
		}
	}
	in MkObject
	{
		objSubscribe = \initialise updater -> do
		{
			((var,firsta),firsttoken,sub) <- subscribe 
			 (\a -> do
			{
				var <- newEmptyMVar;
				return (var,a);
			}) 
			 (\(var,_) token medita -> modifyMVar_ var (\(store,oldstate,_,olda) -> let
			{
				(newa,(newstate,meditb)) = case medita of
				{
					Just edita -> (applyEdit edita olda,lensUpdate lens olda edita oldstate);
					_ -> (olda,(oldstate,Nothing));
				};
			} in do
			{
				pushStore store token meditb;
				return (store,newstate,token,newa);
			}));
			putMVar var (emptyStore,firststate,firsttoken,firsta);
			
			objsub var sub initialise updater;
		}
	};
}
