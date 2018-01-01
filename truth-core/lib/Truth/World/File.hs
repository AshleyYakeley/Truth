module Truth.World.File where

import Truth.Core
import Truth.Core.Import

fileObject :: FilePath -> Object ByteStringEdit
fileObject path = let
    objRun :: UnliftIO (ReaderT Handle IO)
    objRun rt = do
        h <- openBinaryFile path ReadWriteMode
        r <- runReaderT rt h
        hClose h
        return r
    objRead :: MutableRead (ReaderT Handle IO) ByteStringReader
    objRead ReadByteStringLength = do
        h <- ask
        n <- lift $ hFileSize h
        return $ fromInteger n
    objRead (ReadByteStringSection start len) = do
        h <- ask
        lift $ hSeek h AbsoluteSeek $ toInteger start
        lift $ hGet h $ fromIntegral len
    objOneEdit :: ByteStringEdit -> ReaderT Handle IO ()
    objOneEdit (ByteStringSetLength len) = do
        h <- ask
        lift $ hSetFileSize h $ toInteger len
    objOneEdit (ByteStringWrite start bs) = do
        h <- ask
        oldlen <- lift $ hFileSize h
        if toInteger start > oldlen
            then lift $ hSetFileSize h $ toInteger start
            else return ()
        lift $ hSeek h AbsoluteSeek $ toInteger start
        lift $ hPut h bs
    objEdit :: [ByteStringEdit] -> ReaderT Handle IO (Maybe (ReaderT Handle IO ()))
    objEdit = singleAlwaysEdit objOneEdit
    in MkObject {..}
