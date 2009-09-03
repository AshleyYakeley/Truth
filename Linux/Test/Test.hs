{-# LANGUAGE FlexibleContexts,UndecidableInstances,RankNTypes,ScopedTypeVariables #-}
module Main where
{
    import Data.Changes.File.Linux;
    import Data.Changes.Editor;
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
    
    instance (Show a,Show edit) => Show (ListEdit a edit) where
    {
        show (ItemEdit i edit) = "item " ++ (show i) ++ " " ++ show edit;
        show (ReplaceSectionEdit i sect) = "section " ++ (show i) ++ " " ++ show sect;
        show (ReplaceListEdit la) = "replace " ++ (show la);
    };
    
    instance (Show a,Show edit) => Show (JustEdit a edit) where
    {
        show (JustEdit edit) = "Just " ++ (show edit);
        show (ReplaceJustEdit a) = "replace " ++ (show a);
    };
   
    instance (Show a) => Show (WholeEdit a) where
    {
        show (MkWholeEdit s) = "replace " ++ (show s);
    };
   
    showObject :: (Show a) => String -> Subscribe a edit -> IO ();
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
    
    makeShowSubscription :: (Show a,Show edit,EditScheme a edit) => String -> Subscribe a edit -> IO (Push edit,Subscription a edit);
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
    
    showPushEdit :: (Show edit) => Push edit -> edit -> IO ();
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
                textobj :: Subscribe (Maybe (Result ListError String)) (WholeEdit (Maybe (Result ListError String)))
                 = lensSubscribe (fixedFloatingLens (simpleFixedLens (wholeSimpleLens (cfmap (utf8Lens . packBSLens))))) () contentobj;
            };
        
            (push,sub) <- makeShowSubscription path textobj;

            putStrLn "writing & waiting 100ms";
            writeFile path (pack [69,70,71,72,10]);
            threadDelay 100000;

            putStrLn "writing & waiting 100ms";
            writeFile path (pack [73,74,75,10]);
            threadDelay 100000;

            showPushEdit push (MkWholeEdit (Just (SuccessResult "pqrstu")));

    --        writeFile path (pack [65,66,67,68,10]);
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
            showPushEdit push (MkWholeEdit Nothing);
            showPushEdit push (MkWholeEdit Nothing);
            showPushEdit push (MkWholeEdit (Just (SuccessResult "ABCdef")));
            showPushEdit push (MkWholeEdit Nothing);

            subClose sub;
            putStrLn "End";
        });
    };
}
