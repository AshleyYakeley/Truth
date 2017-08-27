module Truth.World.Soup where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Truth.World.FileSystem;
    import System.FilePath;
    import Data.UUID;


    type SoupEdit edit = KeyEdit [(UUID,EditSubject edit)] (PairEdit (NoEdit (WholeReader UUID)) edit);

    liftSoupLens :: forall c f state edita editb. (ReadableConstraint c,Applicative f,Edit edita,Edit editb,FullReader c (EditReader editb)) =>
        (forall m. (Monad m,c m) => EditSubject editb -> m (f (EditSubject edita))) ->
        EditLens' c f state edita editb -> EditLens' c f state (SoupEdit edita) (SoupEdit editb);
    liftSoupLens bmfa = let
    {
        conv :: forall m. (Monad m,c m) => (UUID,EditSubject editb) -> m (f (UUID,EditSubject edita));
        conv (uuid,b) = fmap (fmap $ \a -> (uuid,a)) $ bmfa b;
    } in liftKeyElementLens conv . sndLiftEditLens;

    nameToUUID :: String -> Maybe UUID;
    nameToUUID = Data.UUID.fromString;

    uuidToName :: UUID -> String;
    uuidToName = Data.UUID.toString;

    directorySoupMutableEdit :: MutableEdit IO FSEdit -> FilePath -> MutableEdit (AutoClose FilePath ByteStringEdit) (SoupEdit (MutableIOEdit ByteStringEdit));
    directorySoupMutableEdit fs dirpath = MkMutableEdit
    {
        mutableRead = \r -> case r of
        {
            KeyReadKeys -> do
            {
                mnames <- liftIO $ mutableRead fs $ FSReadDirectory dirpath;
                return $ case mnames of
                {
                    Just names -> mapMaybe nameToUUID $ MkFiniteSet names;
                    Nothing -> mempty;
                }
            };
            KeyReadItem uuid (MkTupleEditReader EditFirst ReadWhole) -> do
            {
                mitem <- liftIO $ mutableRead fs $ FSReadItem $ dirpath </> uuidToName uuid;
                case mitem of
                {
                    Just (FSFileItem _) -> return $ Just uuid;
                    _ -> return Nothing;
                };
            };
            KeyReadItem uuid (MkTupleEditReader EditSecond ReadMutableIO) -> do
            {
                let
                {
                    path = dirpath </> uuidToName uuid;
                };
                mitem <- liftIO $ mutableRead fs $ FSReadItem path;
                case mitem of
                {
                    Just (FSFileItem object) -> do
                    {
                        muted <- acOpenObject path object;
                        return $ Just muted;
                    };
                    _ -> return Nothing;
                };
            };
        },
        mutableEdit = singleMutableEdit $ \edit -> fmap (fmap liftIO) $ liftIO $ case edit of
        {
            KeyEditItem _uuid (MkTupleEdit EditFirst iedit) -> never iedit;
            KeyEditItem _uuid (MkTupleEdit EditSecond iedit) -> never iedit;
            KeyDeleteItem uuid -> mutableEdit fs [FSEditDeleteNonDirectory $ dirpath </> uuidToName uuid];
            KeyInsertReplaceItem (uuid,bs) -> mutableEdit fs [FSEditCreateFile (dirpath </> uuidToName uuid) bs];
            KeyClear -> do
            {
                mnames <- mutableRead fs $ FSReadDirectory dirpath;
                return $ case mnames of
                {
                    Just names -> Just $ for_ names $ \name -> mutableEdit fs [FSEditDeleteNonDirectory $ dirpath </> name];
                    Nothing -> Nothing;
                }
            }
        }
    };

    directorySoup :: MutableEdit IO FSEdit -> FilePath -> Object (SoupEdit (MutableIOEdit ByteStringEdit));
    directorySoup muted dirpath = MkObject $ \call -> runAutoClose $ call $ directorySoupMutableEdit muted dirpath;
}
