module Truth.World.Soup where
{
    import Truth.Core.Import;
    import Truth.Core;
    import Truth.World.FileSystem;
    import System.FilePath;
    import Data.UUID;


    type SoupEdit edit = KeyEdit [(UUID,EditSubject edit)] (PairEdit (NoEdit (WholeReader UUID)) edit);

    nameToUUID :: String -> Maybe UUID;
    nameToUUID = Data.UUID.fromString;

    uuidToName :: UUID -> String;
    uuidToName = Data.UUID.toString;

    directorySoup :: MutableEdit IO FSEdit -> FilePath -> MutableEdit IO (SoupEdit (ObjectEdit ByteStringEdit));
    directorySoup fs dirpath = MkMutableEdit
    {
        mutableRead = \r -> case r of
        {
            KeyReadKeys -> do
            {
                mnames <- mutableRead fs $ FSReadDirectory dirpath;
                return $ case mnames of
                {
                    Just names -> mapMaybe nameToUUID names;
                    Nothing -> [];
                }
            };
            KeyReadItem uuid (MkTupleEditReader EditFirst ReadWhole) -> do
            {
                mitem <- mutableRead fs $ FSReadItem $ dirpath </> uuidToName uuid;
                case mitem of
                {
                    Just (FSFileItem _) -> return $ Just uuid;
                    _ -> return Nothing;
                };
            };
            KeyReadItem uuid (MkTupleEditReader EditSecond ReadObject) -> do
            {
                mitem <- mutableRead fs $ FSReadItem $ dirpath </> uuidToName uuid;
                case mitem of
                {
                    Just (FSFileItem object) -> return $ Just object;
                    _ -> return Nothing;
                };
            };
        },
        mutableEdit = singleMutableEdit $ \edit -> case edit of
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
}
