{-# LANGUAGE FlexibleContexts,UndecidableInstances,RankNTypes #-}
module Main where
{
    import Data.Changes.List;
    import Data.ConstFunction;
    import Data.Changes.Editor;
    import Data.Changes;
    import Data.Maybe;
    import Data.IORef;

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

    withShowSubscription :: (Show a, Show edit,EditScheme a edit) =>
     Subscribe a edit -> String -> (Subscribe a edit -> (edit -> IO (Maybe ())) -> IO (Maybe b)) -> IO (Maybe b);
    withShowSubscription object name f = subscribeEdit object (MkEditor
    {
        editorInit = \a push -> do
        {
            putStrLn (name ++ ": initial: " ++ (show a));
            ref <- newIORef a;
            return (ref,push);
        },
        editorUpdate = \(ref,push) edit -> do
        {
            putStrLn (name ++ ": update: " ++ (show edit));
            newa <- applyConstFunctionA (applyEdit edit) (readIORef ref);
            putStrLn (name ++ ": update: new value: " ++ (show newa));
            writeIORef ref newa;
        },
        editorDo = \(ref,push) obj -> f obj (\edit -> do
        {
            putStrLn (name ++ ": push: " ++ (show edit));
            result <- push edit;
            case result of
            {
                Just _ -> do
                {
                    putStrLn (name ++ ": push: accepted");
                    newa <- applyConstFunctionA (applyEdit edit) (readIORef ref);
                    putStrLn (name ++ ": push: new value: " ++ (show newa));
                    writeIORef ref newa;
                    putStrLn (name ++ ": push: done");
                };
                _ -> putStrLn (name ++ ": push: rejected");
            };
            return result;
        })
    });
    
    main :: IO ();
    main = do
    {
        putStrLn "Test";
        withShowSubscription (freeObjSubscribe "abcdef" :: Subscribe String (ListEdit String (WholeEdit Char))) "main" (\obj push -> do
        {
            push (replaceEdit "pqrstu");
            showObject "main" obj;

--            push (ReplaceEdit "PQRSTU");
--            showObject "current" obj;

            withShowSubscription (lensSubscribe listSection (2,2) obj) "sect" (\sectobj pushSect -> do
            {
                pushSect (replaceEdit "12");
                showObject "sect" sectobj;
                showObject "main" obj;
        
                withShowSubscription (lensSubscribe listElement 4 obj) "elem" (\elemobj pushElem -> do
                {        
                    --pushElem (ReplaceEdit (Just 'p'));
                    --showObject "elem" elemobj;
                    --showObject "sect" sectobj;
                    --showObject "main" obj;

                    pushSect (replaceEdit "x");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;
        
                    pushSect (replaceEdit "ABC");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;
        
                    pushSect (replaceEdit "");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;
        
                    pushSect (replaceEdit "ZUM");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;
        
                    pushElem (replaceEdit (Just 'Q'));
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
