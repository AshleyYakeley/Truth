module Truth.Core.Object.File where
{
    import Truth.Core.Import;
    import Data.ByteString;
    import Truth.Core.Read;
    import Truth.Core.Types;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.LockAPI;


    fileLockAPI :: FilePath -> LockAPI ByteStringEdit ();
    fileLockAPI path = MkLockAPI $ \ff -> do
    {
        h <- openBinaryFile path ReadWriteMode;
        let
        {
            mutableRead :: MutableRead IO ByteStringReader;
            mutableRead ReadByteStringLength = fmap fromInteger $ hFileSize h;
            mutableRead (ReadByteStringSection start len) = do
            {
                hSeek h AbsoluteSeek $ toInteger start;
                hGet h len;
            };
            mutableAllowed _ = return True;
            mutableEdit' (ByteStringSetLength len) = do
            {
                hSetFileSize h $ toInteger len;
                return $ Just ();
            };
            mutableEdit' (ByteStringWrite start bs) = do
            {
                oldlen <- hFileSize h;
                if toInteger start > oldlen
                 then hSetFileSize h $ toInteger start
                 else return ();
                hSeek h AbsoluteSeek $ toInteger start;
                hPut h bs;
                return $ Just ();
            };

            mutableEdit = singleMutableEdit mutableEdit';
        };
        r <- ff () MkMutableEdit{..};
        hClose h;
        return r;
    };
}
