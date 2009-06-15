{-# LANGUAGE FlexibleContexts,UndecidableInstances,RankNTypes #-}
module Main where
{
	import Data.Changes.File.Linux;
	import Data.Changes;
	import Data.ConstFunction;
	import Data.ByteString hiding (putStrLn);
	import Data.Char;
	import Data.Chain;
	import Data.IORef;
	import Data.Result;
	import Control.Concurrent;
	import Control.Category;
	import Prelude hiding (readFile,writeFile,id,(.));
	
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
	
	instance Show ListError where
	{
		show (MkListError i) = "error at item " ++ (show i);
	};
	
	instance (Show e,Show a) => Show (Result e a) where
	{
		show (FailureResult e) = "failure: " ++ (show e);
		show (SuccessResult a) = "success: " ++ (show a);
	};
	
	makeShowSubscription :: (Show a,Show (PartEdit a),Editable a) => String -> Subscribe a -> IO (Push a,Subscription a);
	makeShowSubscription name subscribe = do
	{
		((_,push),sub) <- subscribe
			(\a push -> do
			{
				putStrLn (name ++ ": initial " ++ (show a));
				ref <- newIORef a;
				return (ref,push);
			}) 
			(\(ref,_) edit -> do
			{
				putStrLn (name ++ ": edit: " ++ (show edit));
				newa <- applyConstFunctionA (applyEdit edit) (readIORef ref);
				putStrLn (name ++ ": update: " ++ (show newa));
				writeIORef ref newa;
			});
		return (push,sub);
	};
	
	showPushEdit :: (Show a,Show (PartEdit a)) => Push a -> Edit a -> IO ();
	showPushEdit push edit = do
	{
		putStrLn ("pushing " ++ (show edit));
		mm <- push edit;
		case mm of
		{
			Just _ -> putStrLn "pushed";
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
			let
			{
				fileobj = linuxFileObject inotify path;
				contentobj = lensSubscribe (toFloatingLens (fixedFloatingLens (cleanFixedLens contentCleanLens))) () fileobj;
				textobj = lensSubscribe (fixedFloatingLens (simpleFixedLens (wholeSimpleLens (cfmap (utf8Lens . packBSLens))))) () contentobj;
			};
		
			(push,sub) <- makeShowSubscription path textobj;

			putStrLn "writing & waiting 100ms";
			writeFile path (pack [69,70,71,72,10]);
			threadDelay 100000;

			putStrLn "writing & waiting 100ms";
			writeFile path (pack [73,74,75,10]);
			threadDelay 100000;

			showPushEdit push (ReplaceEdit (Just (SuccessResult "pqrstu")));

	--		writeFile path (pack [65,66,67,68,10]);
	{-		
			obj <- freeObjSubscribe () "abcdef";
			sub <- makeShowSubscription "main" obj;
			showPushEdit sub (ReplaceEdit "pqrstu");
		
			showObject "current" obj;
		
			let {sectobj = lensSubscribe listSection (\_ -> return (2,2)) obj;};
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

			putStrLn "waiting 10s";
			threadDelay 10000000;

			putStrLn "deleting";
			showPushEdit push (ReplaceEdit Nothing);
			showPushEdit push (ReplaceEdit Nothing);
			showPushEdit push (ReplaceEdit (Just (SuccessResult "ABCdef")));
			showPushEdit push (ReplaceEdit Nothing);

			subClose sub;
			putStrLn "End";
		});
	};
}
