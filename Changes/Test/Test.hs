module Main where
{
	import Data.Changes;
	import Data.IORef;
	
	showEdit :: (Show s) => Edit s -> String;
	showEdit (ReplaceEdit s) = "replace " ++ (show s);
	showEdit _ = "lens";
	
	
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
		
		sub2 <- makeShowSubscription "copy" obj;
		
		let {sectobj = lensObject listSection (\_ -> return (2,2)) obj;};
		sectsub <- makeShowSubscription "sect" sectobj;		
		
		
		subClose sub;
		subClose sectsub;
		putStrLn "End";
	};
}
