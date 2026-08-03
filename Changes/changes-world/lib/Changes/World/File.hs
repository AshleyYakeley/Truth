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
    objRun :: ResourceRunner '[Handle]
    objRun =
        mkResourceRunner iow $ \call -> do
            h <- openBinaryFile path ReadWriteMode
            r <- call h
            hClose h
            return r
    refRead :: Readable (ReaderT (ListProduct '[Handle]) IO) ByteStringReader
    refRead ReadByteStringLength = do
        h <- asks fst
        n <- lift $ hFileSize h
        return $ fromInteger n
    refRead (ReadByteStringSection start len) = do
        h <- asks fst
        lift $ hSeek h AbsoluteSeek $ toInteger start
        lift $ hGet h $ fromIntegral len
    objOneEdit :: ByteStringEdit -> EditSource -> ReaderT (ListProduct '[Handle]) IO ()
    objOneEdit (ByteStringSetLength len) _ = do
        h <- asks fst
        lift $ hSetFileSize h $ toInteger len
    objOneEdit (ByteStringWrite start bs) _ = do
        h <- asks fst
        oldlen <- lift $ hFileSize h
        if toInteger start > oldlen
            then lift $ hSetFileSize h $ toInteger start
            else return ()
        lift $ hSeek h AbsoluteSeek $ toInteger start
        lift $ hPut h bs
    refEdit ::
        NonEmpty ByteStringEdit ->
        ReaderT (ListProduct '[Handle]) IO (Maybe (EditSource -> ReaderT (ListProduct '[Handle]) IO ()))
    refEdit = singleAlwaysEdit objOneEdit
    refCommitTask :: Task IO ()
    refCommitTask = mempty
    in MkResource objRun MkAReference{..}
