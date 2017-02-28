module Truth.Object.Subscribe  where
{
    import Truth.Edit.Import;
    import Truth.Edit;

    {-

    A reference to a value of type t, structured by reader and edit

    * At any time has a value. This may be expensive to retrieve in its entirety.
    * May be updated by some other process. Holder can subscribe to notifications of these.
    * Holder can read from it (per reader). But some reads may be expensive or block.
    * Holder can change it (per edit). But some edits may not be allowed.
    * Holder can retrieve a pure function (edit -> Bool) that gives which changes are allowed at that time.

    Example:
    A file is a reference. It may be large.
    t = [Word8] or ByteString
    reader = ListBlockRead Word8 or ByteStringBlockRead
    edit = ListBlockEdit Word8 or ByteStringBlockEdit

    Example:
    The interpretation of another reference, using a codec.
    * It may be updated to be "decode failure".
    * Holder edit to "decode failure" is not allowed.


    -}


    type Partial reader = Structure Maybe reader;

    emptyPartial :: Partial reader;
    emptyPartial _reader = Nothing;

    -- mapStructure (applyEdit edit) :: Partial reader -> Partial reader;


    {-
    -- view tells thing:
    readFromThing :: thing -> Structure IO reader
    changeThing :: thing -> edit -> IO Bool
    canChangeThing :: thing -> edit -> IO Bool
    close :: thing -> IO ()

    -- thing tells view:
    initial :: Partial reader -> IO r
    update :: r -> Partial reader -> IO ()
    -}

    {-
    file: subject :: [Word8]
    text: subject :: String
    section1: subject :: String
    section2: subject :: String

    with local copies:
    section1 change -> text change
    text change -> section2 change (if necessary)
    text change -> file change
    file change = write to file

    without local copies:
    section1 change -> text change
    text change -> file change
    file change = write to file
    write to file -> file update
    file update (entire file) -> text update
    text update -> section1 update
    text update -> section2 update (regardless)


    partial objects: Structure Maybe reader
    -}


    type Send edit = edit -> IO (Maybe (Allowed edit));
    type Allowed edit = edit -> Bool;

    data Subscription t edit = MkSubscription
    {
        subCopy :: Subscribe t edit,

        -- | close the subscription
        subClose :: IO ()
    };

    data State edit = MkState
    {
        stateValue :: Partial (EditReader edit),
        stateCanChange :: Allowed edit
    };

    type Subscribe t message = forall r.
        (t -> Allowed message -> Send message -> IO r) -> -- initialise
        (r -> message -> Allowed message -> IO ()) -> -- receive
        IO (r, Subscription t message);

    data Object t message = MkObject
    {
        objGetInitial :: forall r. (Allowed message -> IO r) -> IO r,
        objRead :: t, -- Structure IO (EditReader edit),
        objSend :: Send message,
        objClose :: IO ()
    };

    newtype Subscribe' t message = MkSubscribe' {unsubscribe' :: Subscribe t message};

    objectGetSubscribe :: forall t message.
     ((message -> Allowed message -> IO ()) -> IO (Object t message)) -> IO (Subscribe' t message);
    objectGetSubscribe getObject = do
    {
        storevar <- newMVar (emptyStore :: Store (message -> Allowed message -> IO ()));
        let
        {
            sendDown :: message -> Allowed message -> IO ();
            sendDown message allowed = withMVar storevar $ \store -> do
            {
                _ <- for (allStore store) (\u -> u message allowed);
                return ();
            };
        };
        obj <- getObject sendDown;
        let
        {
            keySendUp :: Int -> Send message;
            keySendUp key message = withMVar storevar $ \store -> do
            {
                mv <- objSend obj message;
                case mv of
                {
                    Just allowed -> for (allStoreExcept store key) (\u -> u message allowed) >> return ();
                    _ -> return ();
                };
                return mv;
            };

            keyClose :: Int -> IO ();
            keyClose key = modifyMVar_ storevar $ \store -> let
            {
                newstore = deleteStore key store;
            } in do
            {
                if isEmptyStore newstore
                 then (objClose obj)
                 else return ();
                return newstore;
            };

            keySubscription :: Int -> Subscription t message;
            keySubscription key = MkSubscription
            {
                subCopy = objSub,
                subClose = keyClose key
            };

            objSub :: Subscribe t message;
            objSub initialise updater = mfix $ \result -> do
            {
                key <- modifyMVar storevar $ \store -> do
                {
                    let {(key,newstore) = addStore (updater (fst result)) store;};
                    return (newstore,key);
                };
                r <- objGetInitial obj $ \allowed -> initialise (objRead obj) allowed (keySendUp key);
                return (r,keySubscription key);
            };
        };
        return $ MkSubscribe' objSub;
    };

    objSubscribe :: forall t message.
     ((message -> Allowed message -> IO ()) -> IO (Object t message)) -> Subscribe t message;
    objSubscribe getObject initialise recieve = do
    {
        MkSubscribe' sub <- objectGetSubscribe getObject;
        sub initialise recieve;
    };
}
