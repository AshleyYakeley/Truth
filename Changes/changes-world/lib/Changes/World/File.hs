module Changes.World.File
    ( fileReference
    )
where

import Changes.Core
import Shapes

fileWitness :: IOWitness Handle
fileWitness = $(iowitness [t|Handle|])

fileReference :: FilePath -> Reference ByteStringEdit
fileReference path = let
    iow :: IOWitness Handle
    iow = hashOpenWitness fileWitness path
    objRun :: ResourceRunner Handle
    objRun =
        mkResourceRunner iow $ \call -> do
            h <- openBinaryFile path ReadWriteMode
            r <- call h
            hClose h
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
        when (toInteger start > oldlen) $ lift $ hSetFileSize h $ toInteger start
        lift $ hSeek h AbsoluteSeek $ toInteger start
        lift $ hPut h bs
    refEdit ::
        NonEmpty ByteStringEdit ->
        ReaderT Handle IO (Maybe (EditSource -> ReaderT Handle IO ()))
    refEdit = singleAlwaysEdit objOneEdit
    refCommitTask :: Task IO ()
    refCommitTask = mempty
    in MkResource objRun MkAReference{..}
