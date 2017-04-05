module Truth.Core.Object.File where
{
    import Truth.Core.Import;
    import Data.ByteString;
    import Truth.Core.Read;
    import Truth.Core.Types;
    import Truth.Core.Object.Object;


    fileLockAPI :: FilePath -> LockAPI ByteStringEdit ();
    fileLockAPI path = MkLockAPI $ \ff -> do
    {
        h <- openBinaryFile path ReadWriteMode;
        let
        {
            apiRead :: Structure IO ByteStringReader;
            apiRead ReadByteStringLength = fmap fromInteger $ hFileSize h;
            apiRead (ReadByteStringSection start len) = do
            {
                hSeek h AbsoluteSeek $ toInteger start;
                hGet h len;
            };
            apiAllowed _ = return True;
            apiEdit' (ByteStringSetLength len) = do
            {
                hSetFileSize h $ toInteger len;
                return $ Just ();
            };
            apiEdit' (ByteStringWrite start bs) = do
            {
                oldlen <- hFileSize h;
                if toInteger start > oldlen
                 then hSetFileSize h $ toInteger start
                 else return ();
                hSeek h AbsoluteSeek $ toInteger start;
                hPut h bs;
                return $ Just ();
            };

            apiEdit = singleApiEdit apiEdit';
        };
        r <- ff () MkAPI{..};
        hClose h;
        return r;
    };
}
