module Changes.Core.Types.ByteString
    ( ByteStringReader(..)
    , ByteStringEdit(..)
    , ByteStringUpdate
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Read

data ByteStringReader t where
    ReadByteStringLength :: ByteStringReader Int64
    ReadByteStringSection :: Int64 -> Int64 -> ByteStringReader LazyByteString

instance SubjectReader ByteStringReader where
    type ReaderSubject ByteStringReader = LazyByteString
    -- | Make MutableEdit calls when you've actually got the subject
    mSubjectToReadable msubj ReadByteStringLength = do
        subj <- msubj
        return $ fromIntegral $ olength subj
    mSubjectToReadable msubj (ReadByteStringSection start len) = do
        subj <- msubj
        return $ take len $ drop start subj

instance FullSubjectReader ByteStringReader where
    readableToSubject mr = do
        len <- mr ReadByteStringLength
        mr $ ReadByteStringSection 0 len

data ByteStringEdit
    = ByteStringSetLength Int64
    | ByteStringWrite Int64
                      LazyByteString

instance Floating ByteStringEdit ByteStringEdit

type instance EditReader ByteStringEdit = ByteStringReader

instance ApplicableEdit ByteStringEdit where
    applyEdit (ByteStringSetLength n) _ ReadByteStringLength = return n
    applyEdit (ByteStringSetLength newlen) mr (ReadByteStringSection start len) =
        if start > newlen
            then return mempty
            else do
                let blocklen = min len (newlen - start)
                oldlen <- mr ReadByteStringLength
                let
                    readlen = oldlen - start
                    zerolen = blocklen - readlen
                if readlen < 0
                    then return $ replicate blocklen 0
                    else if zerolen < 0
                             then mr $ ReadByteStringSection start blocklen
                             else do
                                 bs1 <- mr $ ReadByteStringSection start readlen
                                 return $ mappend bs1 $ replicate zerolen 0
    applyEdit (ByteStringWrite w bs) mr ReadByteStringLength = do
        let end = w + fromIntegral (olength bs)
        oldlen <- mr ReadByteStringLength
        return $ max oldlen end
    applyEdit (ByteStringWrite writeStart bs) mr (ReadByteStringSection readStart readLen) = do
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
                then mr $ ReadByteStringSection beforeStart beforeLen
                else return mempty
        afterBS <-
            if afterLen > 0
                then mr $ ReadByteStringSection afterStart afterLen
                else return mempty
        return $ mappend beforeBS $ mappend middleBS afterBS

instance InvertibleEdit ByteStringEdit where
    invertEdit (ByteStringSetLength newlen) mr = do
        oldlen <- mr ReadByteStringLength
        case compare newlen oldlen of
            EQ -> return []
            LT -> do
                bs <- mr $ ReadByteStringSection newlen (oldlen - newlen)
                return $ [ByteStringWrite newlen bs]
            GT -> return $ [ByteStringSetLength oldlen]
    invertEdit (ByteStringWrite writeStart bs) mr = do
        oldLen <- mr ReadByteStringLength
        let
            writeLen = fromIntegral $ olength bs
            writeEnd = writeStart + writeLen
            lenEdit =
                if writeEnd > oldLen
                    then [ByteStringSetLength oldLen]
                    else []
        oldbs <- mr $ ReadByteStringSection writeStart writeLen
        let
            writeEdit =
                if bs == oldbs
                    then []
                    else [ByteStringWrite writeStart oldbs]
        return $ lenEdit ++ writeEdit

chunks :: Integral i => i -> i -> [(i, i)]
chunks csize len = zip [0,csize ..] $ takeWhile ((<) 0) $ fmap (\n -> min csize $ len - (csize * n)) [0 ..]

instance SubjectMapEdit ByteStringEdit

instance FullEdit ByteStringEdit where
    replaceEdit mr write = do
        len <- mr ReadByteStringLength
        write $ ByteStringSetLength len
        for_ (chunks 8192 len) $ \(start, size) -> do
            bs <- mr $ ReadByteStringSection start size
            write $ ByteStringWrite start bs

type ByteStringUpdate = EditUpdate ByteStringEdit
