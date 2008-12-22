module Main where
{
	import Data.Changes;
	import Data.IORef;
	
	showEdit :: (Show s) => Edit s -> String;
	showEdit (ReplaceEdit s) = "replace " ++ (show s);
	showEdit _ = "lens";
	
	showObject :: (Show a) => String -> Object context a -> IO ();
	showObject name obj = do
	{
		a <- readObject obj;
		putStrLn (name ++ ": " ++ (show a));
	};
	
	makeShowSubscription :: (Show a) => String -> Object context a -> IO (Subscription a);
	makeShowSubscription name obj = do
	{
		(_,sub) <- subscribe obj 
			(\a -> do
			{
				putStrLn (name ++ ": initial " ++ (show a));
				newIORef a;
			}) 
			(\ref edit -> do
			{
				putStrLn (name ++ ": edit: " ++ (showEdit edit));
				olda <- readIORef ref;
				let {newa = applyEdit edit olda;};
				putStrLn (name ++ ": update: " ++ (show newa));
				writeIORef ref newa;
			});
		return sub;
	};
	
	showPushEdit :: Subscription a -> Edit a -> IO ();
	showPushEdit sub edit = do
	{
		mio <- subPush sub edit;
		putStrLn "Examining";
		case mio of
		{
			Just io -> do
			{
				putStrLn "pushing";
				io;
				putStrLn "pushed";
			};
			_ -> putStrLn "impossible";
		};
	};
	
	main :: IO ();
	main = do
	{
		putStrLn "Test";
		obj <- makeFreeObject () "abcdef";
		sub <- makeShowSubscription "main" obj;
		showPushEdit sub (ReplaceEdit "pqrstu");
		
		showObject "current" obj;
		
		let {sectobj = lensObject listSection (\_ -> return (2,2)) obj;};
		sectsub <- makeShowSubscription "sect" sectobj;		
		
		showPushEdit sectsub (ReplaceEdit "12");
		showObject "sect" sectobj;
		showObject "main" obj;
		
		showPushEdit sectsub (ReplaceEdit "x");
		showObject "sect" sectobj;
		showObject "main" obj;
		
		showPushEdit sectsub (ReplaceEdit "ABC");
		showObject "sect" sectobj;
		showObject "main" obj;
		
		showPushEdit sectsub (ReplaceEdit "");
		showObject "sect" sectobj;
		showObject "main" obj;
		
		showPushEdit sectsub (ReplaceEdit "ZUM");
		showObject "sect" sectobj;
		showObject "main" obj;
		
		subClose sub;
		subClose sectsub;
		putStrLn "End";
	};
}
