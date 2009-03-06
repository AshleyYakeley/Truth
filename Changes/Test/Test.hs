module Main where
{
	import Data.Changes;
	import Data.Maybe;
	import Data.IORef;
	
	showEdit :: (Show s) => Edit s -> String;
	showEdit (ReplaceEdit s) = "replace " ++ (show s);
	showEdit _ = "lens";
	
	showObject :: (Show a) => String -> Object a -> IO ();
	showObject name obj = do
	{
		a <- readObject obj;
		putStrLn (name ++ ": " ++ (show a));
	};

	withShowSubscription :: (Show a) => Object a -> String -> (Object a -> (Edit a -> IO (Maybe ())) -> IO (Maybe b)) -> IO (Maybe b);
	withShowSubscription object name f = withSubscription object (MkEditor
	{
		editorInit = \a push -> do
		{
			putStrLn (name ++ ": initial " ++ (show a));
			ref <- newIORef a;
			return (ref,push);
		},
		editorUpdate = \(ref,push) edit -> do
		{
			putStrLn (name ++ ": edit: " ++ (showEdit edit));
			olda <- readIORef ref;
			let {newa = applyEdit edit olda;};
			putStrLn (name ++ ": update: " ++ (show newa));
			writeIORef ref newa;
		},
		editorDo = \(ref,push) obj -> f obj (\edit -> do
		{
			putStrLn "pushing";
			result <- push (return edit);
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
		withShowSubscription (makeFreeObject "abcdef") "main" (\obj push -> do
		{
			push (ReplaceEdit "pqrstu");
			showObject "current" obj;

--			push (ReplaceEdit "PQRSTU");
--			showObject "current" obj;

			withShowSubscription (lensObject listSection (2,2) obj) "sect" (\sectobj pushSect -> do
			{
				pushSect (ReplaceEdit "12");
				showObject "sect" sectobj;
				showObject "main" obj;
		
				withShowSubscription (lensObject listElement 4 obj) "elem" (\elemobj pushElem -> do
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
