module Truth.Object.File where
{
    import Truth.Edit.Import;
    import Data.ByteString;
    import Truth.Edit;
    import Truth.Object.Object;

{-
    data API m edit token = MkAPI
    {
        apiRead :: Structure m (EditReader edit),
        apiAllowed :: edit -> m Bool,
        apiEdit :: edit -> m token
    };

    type LockAPI edit token = forall r. (token -> API IO edit token -> IO r) -> IO r;
-}


    fileLockAPI :: FilePath -> LockAPI ByteStringEdit ();
    fileLockAPI path ff = do
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
            };
            apiEdit (ByteStringWrite start bs) = do
            {
                oldlen <- hFileSize h;
                if toInteger start > oldlen
                 then hSetFileSize h $ toInteger start
                 else return ();
                hSeek h AbsoluteSeek $ toInteger start;
                hPut h bs;
            };
        };
        r <- ff () MkAPI{..};
        hClose h;
        return r;
    };
}
