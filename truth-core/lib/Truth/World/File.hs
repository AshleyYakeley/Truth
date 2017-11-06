module Truth.World.File where

import Truth.Core
import Truth.Core.Import

fileObject :: FilePath -> Object ByteStringEdit
fileObject path =
    MkObject $ \call -> do
        h <- openBinaryFile path ReadWriteMode
        let mutableRead :: MutableRead IO ByteStringReader
            mutableRead ReadByteStringLength = fmap fromInteger $ hFileSize h
            mutableRead (ReadByteStringSection start len) = do
                hSeek h AbsoluteSeek $ toInteger start
                hGet h $ fromIntegral len
            mutableEdit' (ByteStringSetLength len) = hSetFileSize h $ toInteger len
            mutableEdit' (ByteStringWrite start bs) = do
                oldlen <- hFileSize h
                if toInteger start > oldlen
                    then hSetFileSize h $ toInteger start
                    else return ()
                hSeek h AbsoluteSeek $ toInteger start
                hPut h bs
            mutableEdit = singleAlwaysMutableEdit mutableEdit'
        r <- call MkMutableEdit {..}
        hClose h
        return r
