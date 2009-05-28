{-# LANGUAGE FlexibleContexts,UndecidableInstances,RankNTypes #-}
module Main where
{
	import Data.Changes.List;
	import Data.ConstFunction;
	import Data.Changes;
	import Data.Maybe;
	import Data.IORef;
	
	instance Show Nothing where
	{
		show = never;
	};
	
	instance (Show a,Show (PartEdit a)) => Show (ListPartEdit a) where
	{
		show (ItemEdit i edit) = "item " ++ (show i) ++ " " ++ show edit;
		show (ReplaceSectionEdit i sect) = "section " ++ (show i) ++ " " ++ show sect;
	};
	
	instance (Show a,Show (PartEdit a)) => Show (JustEdit a) where
	{
		show (JustEdit s) = "Just " ++ (show s);
	};
	
	instance (Show a,Show (PartEdit a)) => Show (Edit a) where
	{
		show (ReplaceEdit s) = "replace " ++ (show s);
		show (PartEdit pe) = "part " ++ (show pe);
	};
	
	showObject :: (Show a) => String -> Subscribe a -> IO ();
	showObject name obj = do
	{
		a <- subscribeRead obj;
		putStrLn (name ++ ": " ++ (show a));
	};

	withShowSubscription :: (Show a,Editable a,Show (PartEdit a)) => Subscribe a -> String -> (Subscribe a -> (Edit a -> IO (Maybe ())) -> IO (Maybe b)) -> IO (Maybe b);
	withShowSubscription object name f = subscribeEdit object (MkEditor
	{
		editorInit = \a push -> do
		{
			putStrLn (name ++ ": initial " ++ (show a));
			ref <- newIORef a;
			return (ref,push);
		},
		editorUpdate = \(ref,push) edit -> do
		{
			putStrLn (name ++ ": edit: " ++ (show edit));
			newa <- applyConstFunctionA (applyEdit edit) (readIORef ref);
			putStrLn (name ++ ": update: " ++ (show newa));
			writeIORef ref newa;
		},
		editorDo = \(ref,push) obj -> f obj (\edit -> do
		{
			putStrLn "pushing";
			result <- push (return (Just edit));
			putStrLn (case result of
			{
				Just _ -> "pushed";
				_ -> "impossible";
			});
			return result;
		})
	});
	
	main :: IO ();
	main = do
	{
		putStrLn "Test";
		withShowSubscription (freeObjSubscribe "abcdef") "main" (\obj push -> do
		{
			push (ReplaceEdit "pqrstu");
			showObject "current" obj;

--			push (ReplaceEdit "PQRSTU");
--			showObject "current" obj;

			withShowSubscription (lensSubscribe listSection (2,2) obj) "sect" (\sectobj pushSect -> do
			{
				pushSect (ReplaceEdit "12");
				showObject "sect" sectobj;
				showObject "main" obj;
		
				withShowSubscription (lensSubscribe listElement 4 obj) "elem" (\elemobj pushElem -> do
				{		
					--pushElem (ReplaceEdit (Just 'p'));
					--showObject "elem" elemobj;
					--showObject "sect" sectobj;
					--showObject "main" obj;

					pushSect (ReplaceEdit "x");
					showObject "elem" elemobj;
					showObject "sect" sectobj;
					showObject "main" obj;
		
					pushSect (ReplaceEdit "ABC");
					showObject "elem" elemobj;
					showObject "sect" sectobj;
					showObject "main" obj;
		
					pushSect (ReplaceEdit "");
					showObject "elem" elemobj;
					showObject "sect" sectobj;
					showObject "main" obj;
		
					pushSect (ReplaceEdit "ZUM");
					showObject "elem" elemobj;
					showObject "sect" sectobj;
					showObject "main" obj;
		
					pushElem (ReplaceEdit (Just 'Q'));
					showObject "elem" elemobj;
					showObject "sect" sectobj;
					showObject "main" obj;

					return (Just ());
				});
			});
		});

		putStrLn "End";
	};
}
