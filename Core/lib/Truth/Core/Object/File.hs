module Truth.Core.Object.File where
{
    import Truth.Core.Import;
    import Data.ByteString;
    import Truth.Core.Read;
    import Truth.Core.Types;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    fileObject :: FilePath -> Object ByteStringEdit ();
    fileObject path = MkObject $ \call -> do
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
            mutableEdit' (ByteStringSetLength len) = hSetFileSize h $ toInteger len;
            mutableEdit' (ByteStringWrite start bs) = do
            {
                oldlen <- hFileSize h;
                if toInteger start > oldlen
                 then hSetFileSize h $ toInteger start
                 else return ();
                hSeek h AbsoluteSeek $ toInteger start;
                hPut h bs;
            };

            mutableEdit = singleMutableEdit mutableEdit';
        };
        r <- call MkMutableEdit{..} unitStateAccess;
        hClose h;
        return r;
    };
}
