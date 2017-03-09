module Truth.Object.File where
{
    import Truth.Edit.Import;
    import Data.ByteString;
    import Truth.Edit;
    import Truth.Object.Object;


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
            apiEdit (ByteStringSetLength len) = do
            {
                hSetFileSize h $ toInteger len;
                return $ Just ();
            };
            apiEdit (ByteStringWrite start bs) = do
            {
                oldlen <- hFileSize h;
                if toInteger start > oldlen
                 then hSetFileSize h $ toInteger start
                 else return ();
                hSeek h AbsoluteSeek $ toInteger start;
                hPut h bs;
                return $ Just ();
            };
        };
        r <- ff () MkAPI{..};
        hClose h;
        return r;
    };
}
