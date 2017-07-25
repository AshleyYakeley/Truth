module File where
{
    import Control.Concurrent.STM;
    import Data.ByteString;
    import System.GIO;
    import Data.Changes;


    refSubscribe :: (Eq a) => ValueType a -> IO a -> (a -> IO ()) -> IO (Subscription a);
    refSubscribe vt getter setter = do
    {
        initial <- getter;
        stateVar <- newTVarIO initial;
        doneVar <- newTVarIO False;

        return (MkSubscription
        {
            subInitial = initial,

            subPull = atomically (do
            {
                done <- readTVar doneVar;
                if (done)
                then return Nothing
                else retry;
            }),

            subPush = \edit -> do
            {
                current <- getter;
                rr <- atomically (do
                {
                    old <- readTVar stateVar;
                    case compareEdit vt old current of
                    {
                        [] -> do
                        {
                            let {new = applyEdit edit old;};
                            writeTVar stateVar new;
                            return (Right new);
                        };
                        curEdits -> do
                        {
                            writeTVar stateVar current;
                            return (Left curEdits);
                        };
                    };

                });
                case rr of
                {
                    Left curEdits -> return (Just curEdits);
                    Right new -> do
                    {
                        setter new;
                        return Nothing;
                    };
                };
            },

            subClose = atomically (writeTVar doneVar True)
        });
    };

    uriObject :: File -> Object (Maybe ByteString);
    uriObject uri = MkObject
    {
        objContext = uri,
        subscribe = refSubscribe (MaybeValueType ByteStringValueType) getURI setURI
    } where
    {
        getURI :: IO (Maybe ByteString);
        getURI = do
        {
            catch (do
            {
bs <- return empty;
{-
                typeInfo <- fileQueryInfo file "" [FileQueryInfoNofollowSymlinks] Nothing;
                h <- fileRead uri Nothing;
                Just fs <- return (fileInfoSize typeInfo);
                bs <- System.Gnome.VFS.read h fs;
                close h;
-}
                return (Just bs);
            })
            (\_ -> return Nothing);
        };

        setURI :: Maybe ByteString -> IO ();
        setURI Nothing = fileDelete uri Nothing;
        setURI (Just _) = do
        {
return ();
{-
            h <- fileCreate uri Nothing;
            write h bs;
            close h;
-}
        };
    };
}
