module Truth.Core.Types.ByteString where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

data ByteStringReader t where
    ReadByteStringLength :: ByteStringReader Int64
    ReadByteStringSection :: Int64 -> Int64 -> ByteStringReader ByteString

instance SubjectReader ByteStringReader where
    type ReaderSubject ByteStringReader = ByteString
    -- | Make MutableEdit calls when you've actually got the subject
    readFromSubjectM msubj ReadByteStringLength = do
        subj <- msubj
        return $ fromIntegral $ olength subj
    readFromSubjectM msubj (ReadByteStringSection start len) = do
        subj <- msubj
        return $ take len $ drop start subj

instance FullSubjectReader ByteStringReader where
    subjectFromReader = do
        len <- readable ReadByteStringLength
        readable $ ReadByteStringSection 0 len

data ByteStringEdit
    = ByteStringSetLength Int64
    | ByteStringWrite Int64
                      ByteString

instance Floating ByteStringEdit ByteStringEdit

instance Edit ByteStringEdit where
    type EditReader ByteStringEdit = ByteStringReader
    applyEdit (ByteStringSetLength n) ReadByteStringLength = return n
    applyEdit (ByteStringSetLength newlen) (ReadByteStringSection start len) =
        if start > newlen
            then return mempty
            else do
                let blocklen = min len (newlen - start)
                oldlen <- readable ReadByteStringLength
                let
                    readlen = oldlen - start
                    zerolen = blocklen - readlen
                if readlen < 0
                    then return $ replicate blocklen 0
                    else if zerolen < 0
                             then readable $ ReadByteStringSection start blocklen
                             else do
                                 bs1 <- readable $ ReadByteStringSection start readlen
                                 return $ mappend bs1 $ replicate zerolen 0
    applyEdit (ByteStringWrite w bs) ReadByteStringLength = do
        let end = w + fromIntegral (olength bs)
        oldlen <- readable ReadByteStringLength
        return $ max oldlen end
    applyEdit (ByteStringWrite writeStart bs) (ReadByteStringSection readStart readLen) = do
        let
            writeLen = fromIntegral $ olength bs
            writeEnd = writeStart + writeLen
            readEnd = readStart + readLen
            beforeStart = readStart
            beforeLen = min (writeStart - readStart) readLen
            middleStart = max readStart writeStart
            middleEnd = min readEnd writeEnd
            middleLen = middleEnd - middleStart
            middleBS =
                if middleLen > 0
                    then take middleLen $ drop (middleStart - readStart) bs
                    else mempty
            afterStart = max readStart writeEnd
            afterEnd = max readEnd writeEnd
            afterLen = afterEnd - afterStart
        beforeBS <-
            if beforeLen > 0
                then readable $ ReadByteStringSection beforeStart beforeLen
                else return mempty
        afterBS <-
            if afterLen > 0
                then readable $ ReadByteStringSection afterStart afterLen
                else return mempty
        return $ mappend beforeBS $ mappend middleBS afterBS

instance InvertibleEdit ByteStringEdit where
    invertEdit (ByteStringSetLength newlen) = do
        oldlen <- readable ReadByteStringLength
        case compare newlen oldlen of
            EQ -> return []
            LT -> do
                bs <- readable $ ReadByteStringSection newlen (oldlen - newlen)
                return $ [ByteStringWrite newlen bs]
            GT -> return $ [ByteStringSetLength oldlen]
    invertEdit (ByteStringWrite writeStart bs) = do
        oldLen <- readable ReadByteStringLength
        let
            writeLen = fromIntegral $ olength bs
            writeEnd = writeStart + writeLen
            lenEdit =
                if writeEnd > oldLen
                    then [ByteStringSetLength oldLen]
                    else []
        oldbs <- readable $ ReadByteStringSection writeStart writeLen
        let
            writeEdit =
                if bs == oldbs
                    then []
                    else [ByteStringWrite writeStart oldbs]
        return $ lenEdit ++ writeEdit

instance FullEdit ByteStringEdit where
    replaceEdit = do
        len <- readable ReadByteStringLength
        bs <- readable $ ReadByteStringSection 0 len
        wrWrite $ ByteStringWrite 0 bs
        wrWrite $ ByteStringSetLength len
