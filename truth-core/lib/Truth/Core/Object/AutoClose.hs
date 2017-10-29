module Truth.Core.Object.AutoClose where
{
    import Truth.Core.Import;
    import Data.Map.Strict hiding (lookup);
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    runObjectIO :: Object edit -> (MutableEdit IO edit -> IO r) -> IO r;
    runObjectIO object call = runObject object $ \muted -> liftWithUnlift $ \unlift -> call $ remonadMutableEdit unlift muted;

    openObject :: Object edit -> IO (MutableEdit IO edit,IO ());
    openObject object = do
    {
        mutedVar <- newEmptyMVar;
        closerVar <- newEmptyMVar;
        doneVar <- newEmptyMVar;
        _ <- forkIO $ do
        {
            runObjectIO object $ \muted -> do
            {
                putMVar mutedVar muted;
                takeMVar closerVar;
            };
            putMVar doneVar ();
        };
        muted <- takeMVar mutedVar;
        let
        {
            close :: IO ();
            close = do
            {
                putMVar closerVar ();
                takeMVar doneVar;
            };
        };
        return (muted,close);
    };

    type AutoClose key edit = StateT (Map key (MutableEdit IO edit,IO ())) IO;

    runAutoClose :: Ord key => AutoClose key edit a -> IO a;
    runAutoClose ac = do
    {
        (a,mp) <- runStateT ac mempty;
        for_ (elems mp) snd;
        return a;
    };

    acOpenObject :: Ord key => key -> Object edit -> AutoClose key edit (MutableEdit IO edit);
    acOpenObject key object = do
    {
        oldmap <- get;
        case lookup key oldmap of
        {
            Just mutedcloser -> return $ fst mutedcloser;
            Nothing -> do
            {
                mutedcloser <- lift $ openObject object;
                put $ insert key mutedcloser oldmap;
                return $ fst mutedcloser;
            }
        };
    };
}
