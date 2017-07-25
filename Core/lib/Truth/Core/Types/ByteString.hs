module Truth.Core.Types.ByteString where
{
    import Truth.Core.Import;
    import Data.ByteString as BS;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data ByteStringReader t where
    {
        ReadByteStringLength :: ByteStringReader Int;
        ReadByteStringSection :: Int -> Int -> ByteStringReader ByteString;
    };

    instance Reader ByteStringReader where
    {
        type ReaderSubject ByteStringReader = ByteString;

        -- | Make MutableEdit calls when you've actually got the subject
        readFromM msubj ReadByteStringLength = do
        {
            subj <- msubj;
            return $ BS.length subj;
        };
        readFromM msubj (ReadByteStringSection start len) = do
        {
            subj <- msubj;
            return $ BS.take len $ BS.drop start subj;
        };
    };

    instance IOFullReader ByteStringReader;
    instance FullReader ByteStringReader where
    {
        fromReader = do
        {
            len <- readable ReadByteStringLength;
            readable $ ReadByteStringSection 0 len;
        };
    };

    $(return []);
    instance HasTypeInfo ByteStringReader where
    {
        typeWitness = $(generateWitness [t|ByteStringReader|]);
        typeName _ = "ByteStringReader";
        typeKnowledge _ = $(declInfo [d|
            instance Reader ByteStringReader where
            {
                type ReaderSubject ByteStringReader = ByteString;
            };
            instance IOFullReader ByteStringReader;
            instance FullReader ByteStringReader;
        |]);
    };

    data ByteStringEdit = ByteStringSetLength Int | ByteStringWrite Int ByteString;

    instance Floating ByteStringEdit ByteStringEdit;

    instance Edit ByteStringEdit where
    {
        type EditReader ByteStringEdit = ByteStringReader;
        applyEdit (ByteStringSetLength n) ReadByteStringLength = return n;
        applyEdit (ByteStringSetLength newlen) (ReadByteStringSection start len) = if start > newlen
        then return BS.empty
        else do
        {
            let
            {
                blocklen = min len (newlen - start);
            };
            oldlen <- readable ReadByteStringLength;
            let
            {
                readlen = oldlen - start;
                zerolen = blocklen - readlen;
            };
            if readlen < 0
            then return $ BS.replicate blocklen 0;
            else if zerolen < 0
            then readable $ ReadByteStringSection start blocklen
            else do
            {
                bs1 <- readable $ ReadByteStringSection start readlen;
                return $ mappend bs1 $ BS.replicate zerolen 0;
            };
        };
        applyEdit (ByteStringWrite w bs) ReadByteStringLength = do
        {
            let
            {
                end = w + BS.length bs;
            };
            oldlen <- readable ReadByteStringLength;
            return $ max oldlen end;
        };
        applyEdit (ByteStringWrite writeStart bs) (ReadByteStringSection readStart readLen) = do
        {
            let
            {
                writeLen = BS.length bs;
                writeEnd = writeStart + writeLen;
                readEnd = readStart + readLen;

                beforeStart = readStart;
                beforeLen = min (writeStart - readStart) readLen;

                middleStart = max readStart writeStart;
                middleEnd = min readEnd writeEnd;
                middleLen = middleEnd - middleStart;
                middleBS = if middleLen > 0 then BS.take middleLen $ BS.drop (middleStart - readStart) bs else BS.empty;

                afterStart = max readStart writeEnd;
                afterEnd = max readEnd writeEnd;
                afterLen = afterEnd - afterStart;
            };
            beforeBS <- if beforeLen > 0 then readable $ ReadByteStringSection beforeStart beforeLen else return BS.empty;
            afterBS <- if afterLen > 0 then readable $ ReadByteStringSection afterStart afterLen else return BS.empty;
            return $ mappend beforeBS $ mappend middleBS afterBS;
        };

        invertEdit (ByteStringSetLength newlen) = do
        {
            oldlen <- readable ReadByteStringLength;
            case compare newlen oldlen of
            {
                EQ -> return [];
                LT -> do
                {
                    bs <- readable $ ReadByteStringSection newlen (oldlen - newlen);
                    return $ [ByteStringWrite newlen bs];
                };
                GT -> return $ [ByteStringSetLength oldlen];
            };
        };
        invertEdit (ByteStringWrite writeStart bs) = do
        {
            oldLen <- readable ReadByteStringLength;
            let
            {
                writeLen = BS.length bs;
                writeEnd = writeStart + writeLen;

                lenEdit = if writeEnd > oldLen then [ByteStringSetLength oldLen] else [];
            };
            oldbs <- readable $ ReadByteStringSection writeStart writeLen;
            let
            {
                writeEdit = if bs == oldbs then [] else [ByteStringWrite writeStart oldbs];
            };
            return $ lenEdit ++ writeEdit;
        }
    };

    instance IOFullEdit ByteStringEdit;
    instance FullEdit ByteStringEdit where
    {
        replaceEdit = do
        {
            len <- readable ReadByteStringLength;
            bs <- readable $ ReadByteStringSection 0 len;
            wrWrite $ ByteStringWrite 0 bs;
            wrWrite $ ByteStringSetLength len;
        };
    };

    $(return []);
    instance HasTypeInfo ByteStringEdit where
    {
        typeWitness = $(generateWitness [t|ByteStringEdit|]);
        typeName _ = "ByteStringEdit";
        typeKnowledge _ = mconcat [$(declInfo [d|
            instance Edit ByteStringEdit where
            {
                type EditReader ByteStringEdit = ByteStringReader;
            };
        |]),typeInfoKnowledge (typeInfo @ByteStringReader)];
    };
}
