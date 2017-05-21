{-# LANGUAGE FlexibleContexts,UndecidableInstances,RankNTypes #-}
module Main where
{
    import Data.IORef;
    import Data.Maybe;
    import Data.ConstFunction;
    import Truth.Edit;
    import Truth.Subscription;
    import Truth.Subscription.Editor;


    instance (Show edit,Show (Index c)) => Show (IndexEdit c edit) where
    {
        show (MkIndexEdit i edit) = (show i) ++ " " ++ show edit;
    };

    instance (Show (EditSubject edit),Show edit) => Show (ListEdit edit) where
    {
        show (ItemEdit edit) = "item " ++ show edit;
        show (ReplaceSectionEdit i sect) = "section " ++ (show i) ++ " " ++ show sect;
        show (ReplaceListEdit la) = "replace " ++ (show la);
    };

    instance (Show (f (EditSubject edit)),Show edit) => Show (OneEdit f edit) where
    {
        show (MkOneEdit edit) = show edit;
    };

    instance (Show a) => Show (WholeEdit a) where
    {
        show (MkWholeEdit s) = show s;
    };

    instance Show ListRegion where
    {
        show (MkListRegion a b) = (show a) ++ "+" ++ (show b);
    };

    showObject :: (Show (EditSubject edit)) => String -> Subscribe edit -> IO ();
    showObject name obj = do
    {
        a <- subscribeRead obj;
        putStrLn (name ++ ": " ++ (show a));
    };

    withShowSubscription :: (Show (EditSubject edit), Show edit,Edit edit) =>
     Subscribe edit -> String -> (Subscribe edit -> (edit -> IO (Maybe ())) -> IO (Maybe b)) -> IO (Maybe b);
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

        putStrLn ("=======");
        putStrLn "Test Drop";
        withShowSubscription (freeObjSubscribe "pq" :: Subscribe (ListEdit (WholeEdit Char))) "main" (\obj push -> do
        {
            withShowSubscription (mapSubscription (listDrop 2) obj) "sect" (\sectobj pushSect -> do
            {
                putStrLn ("-------");
                push (ReplaceSectionEdit (MkListRegion 2 0) "ZUM");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceListEdit "");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceSectionEdit (MkListRegion 0 0) "M");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceListEdit "");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceListEdit "K");
                showObject "sect" sectobj;
                showObject "main" obj;

                return (Just ());
            });
        });

        putStrLn ("=======");
        putStrLn "Test Take";
        withShowSubscription (freeObjSubscribe "tu" :: Subscribe (ListEdit (WholeEdit Char))) "main" (\obj push -> do
        {
            withShowSubscription (mapSubscription (listTake 0) obj) "sect" (\sectobj pushSect -> do
            {
                putStrLn ("-------");
                push (ReplaceSectionEdit (MkListRegion 0 0) "ZUM");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceListEdit "");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceSectionEdit (MkListRegion 0 0) "M");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceListEdit "");
                showObject "sect" sectobj;
                showObject "main" obj;

                putStrLn ("-------");
                push (ReplaceListEdit "K");
                showObject "sect" sectobj;
                showObject "main" obj;

                return (Just ());
            });
        });

        putStrLn ("=======");
        putStrLn "Test Section";
        withShowSubscription (freeObjSubscribe "abcdef" :: Subscribe (ListEdit (WholeEdit Char))) "main" (\obj push -> do
        {
            putStrLn ("-------");
            push (replaceEdit "pqrstu");
            showObject "main" obj;

--            push (ReplaceEdit "PQRSTU");
--            showObject "main" obj;

            withShowSubscription (mapSubscription (listSection (MkListRegion 2 2)) obj) "sect" (\sectobj pushSect -> do
            {
                putStrLn ("-------");
                pushSect (replaceEdit "12");
                showObject "sect" sectobj;
                showObject "main" obj;

                withShowSubscription (mapSubscription (listElement 4) obj) "elem" (\elemobj pushElem -> do
                {
                    --pushElem (ReplaceEdit (Just 'p'));
                    --showObject "elem" elemobj;
                    --showObject "sect" sectobj;
                    --showObject "main" obj;

                    putStrLn ("-------");
                    pushSect (replaceEdit "x");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    pushSect (replaceEdit "ABC");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    pushSect (replaceEdit "");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    pushSect (replaceEdit "ZUM");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    pushSect (ItemEdit (MkIndexEdit 1 (replaceEdit 'g')));
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    pushElem (replaceEdit (Just 'Q'));
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    push (ItemEdit (MkIndexEdit 5 (replaceEdit 'n')));
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    push (ReplaceListEdit "abcdefgh");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    pushSect (ReplaceSectionEdit (MkListRegion 1 0) "X");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    push (ReplaceSectionEdit (MkListRegion 3 0) "Y");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    push (ReplaceListEdit "");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    push (ReplaceSectionEdit (MkListRegion 0 0) "M");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    push (ReplaceListEdit "");
                    showObject "elem" elemobj;
                    showObject "sect" sectobj;
                    showObject "main" obj;

                    putStrLn ("-------");
                    push (ReplaceListEdit "K");
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
