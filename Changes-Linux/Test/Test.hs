module Main where
{
	import Data.Changes.File.Linux;
	import Data.Changes;
	import Data.ByteString hiding (putStrLn);
	import Data.Char;
	import Data.IORef;
	import Control.Concurrent;
	import Prelude hiding (readFile,writeFile);
	
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
	
	path = "somefile";
	
	main :: IO ();
	main = do
	{
		putStrLn "Test";
		putStrLn "initINotify";
		withINotifyB (\inotify -> do
		{
			writeFile path (pack [65,66,67,68,10]);
			fileobj <- return (linuxFileObject inotify path);
		
			sub <- makeShowSubscription path fileobj;		

			putStrLn "writing & waiting 100ms";
			writeFile path (pack [69,70,71,72,10]);
			threadDelay 100000;

			putStrLn "writing & waiting 100ms";
			writeFile path (pack [73,74,75,10]);
			threadDelay 100000;

			showPushEdit sub (ReplaceEdit (Just (pack (fmap (fromIntegral . ord) "pqrstu"))));

	--		writeFile path (pack [65,66,67,68,10]);
	{-		
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
		
			subClose sectsub;
	-}
			subClose sub;
			putStrLn "End";
		});
	};
}
