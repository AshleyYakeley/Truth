module Truth.World.File
    ( fileReference
    ) where

import Shapes
import Truth.Core

fileWitness :: IOWitness (ReaderT Handle)
fileWitness = $(iowitness [t|ReaderT Handle|])

fileReference :: FilePath -> Reference ByteStringEdit
fileReference path = let
    iow :: IOWitness (ReaderT Handle)
    iow = hashOpenWitness fileWitness path
    objRun :: ResourceRunner '[ ReaderT Handle]
    objRun =
        mkResourceRunner iow $ \rt -> do
            h <- liftIO $ openBinaryFile path ReadWriteMode
            r <- runReaderT rt h
            liftIO $ hClose h
            return r
    refRead :: Readable (ReaderT Handle IO) ByteStringReader
    refRead ReadByteStringLength = do
        h <- ask
        n <- lift $ hFileSize h
        return $ fromInteger n
    refRead (ReadByteStringSection start len) = do
        h <- ask
        lift $ hSeek h AbsoluteSeek $ toInteger start
        lift $ hGet h $ fromIntegral len
    objOneEdit :: ByteStringEdit -> EditSource -> ReaderT Handle IO ()
    objOneEdit (ByteStringSetLength len) _ = do
        h <- ask
        lift $ hSetFileSize h $ toInteger len
    objOneEdit (ByteStringWrite start bs) _ = do
        h <- ask
        oldlen <- lift $ hFileSize h
        if toInteger start > oldlen
            then lift $ hSetFileSize h $ toInteger start
            else return ()
        lift $ hSeek h AbsoluteSeek $ toInteger start
        lift $ hPut h bs
    refEdit :: NonEmpty ByteStringEdit -> ReaderT Handle IO (Maybe (EditSource -> ReaderT Handle IO ()))
    refEdit = singleAlwaysEdit objOneEdit
    refCommitTask :: Task ()
    refCommitTask = mempty
    in MkResource objRun MkAnReference {..}
