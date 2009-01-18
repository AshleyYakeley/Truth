module Main where
{
	import Data.Changes;
	import Data.Maybe;
	import Data.IORef;
	
	showEdit :: (Show s) => Edit s -> String;
	showEdit (ReplaceEdit s) = "replace " ++ (show s);
	showEdit _ = "lens";
	
	showObject :: (Show a) => String -> Object context a -> IO ();
	showObject name obj = do
	{
		ma <- readObject obj;
		putStrLn (name ++ ": " ++ (case ma of
		{
			Just a -> show a;
			_ -> "unsync";
		}));
	};

	withShowSubscription :: (Show a) => Object context a -> String -> ((Edit a -> IO (Maybe (Maybe ()))) -> IO (Maybe b)) -> IO (Maybe b);
	withShowSubscription object name f = withSubscription object (MkEditor_
	{
		editorInit = \a -> do
		{
			putStrLn (name ++ ": initial " ++ (show a));
			newIORef (a,Nothing);
		},
		editorUpdate = \ref newtoken edit -> do
		{
			putStrLn (name ++ ": edit: " ++ (showEdit edit));
			(olda,_) <- readIORef ref;
			let {newa = applyEdit edit olda;};
			putStrLn (name ++ ": update: " ++ (show newa));
			writeIORef ref (newa,Just newtoken);
		},
		editorDo = \ref oldtoken push -> f (\edit -> do
		{
			putStrLn "Examining";
			putStrLn "pushing";
			(_,mnewtoken) <- readIORef ref;
			result <- push (fromMaybe oldtoken mnewtoken) edit;
			putStrLn (case result of
			{
				Just (Just _) -> "pushed";
				Just _ -> "impossible";
				_ -> "unsync";
			});
			return result;
		})
	});
	
	main :: IO ();
	main = do
	{
		putStrLn "Test";
		obj <- makeFreeObject () "abcdef";
		withShowSubscription obj "main" (\push -> do
		{
			push (ReplaceEdit "pqrstu");
			showObject "current" obj;

--			push (ReplaceEdit "PQRSTU");
--			showObject "current" obj;

			let {sectobj = lensObject listSection (\_ -> return (2,2)) obj;};
			withShowSubscription sectobj "sect" (\pushSect -> do
			{
				pushSect (ReplaceEdit "12");
				showObject "sect" sectobj;
				showObject "main" obj;
		
				pushSect (ReplaceEdit "x");
				showObject "sect" sectobj;
				showObject "main" obj;
		
				pushSect (ReplaceEdit "ABC");
				showObject "sect" sectobj;
				showObject "main" obj;
		
				pushSect (ReplaceEdit "");
				showObject "sect" sectobj;
				showObject "main" obj;
		
				pushSect (ReplaceEdit "ZUM");
				showObject "sect" sectobj;
				showObject "main" obj;

				return (Just ());
			});
		});

		putStrLn "End";
	};
}
