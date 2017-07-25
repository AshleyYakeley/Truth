{-# OPTIONS_GHC -fno-warn-orphans #-}
module Truth.Core.Text where
{
    import Truth.Core.Import;


    packBijection :: Bijection ByteString [Word8];
    packBijection = MkBijection unpack pack;

    data ListError = MkListError Int;

    instance Show ListError where
    {
        show (MkListError i) = "decode error at byte " ++ show i;
    };

    instance HasInfo ListError where
    {
        typeWitness = $(generateWitness [t|ListError|]);
        typeName _ = "ListError";
    };

    utf8Injection :: Injection [Word8] (Result ListError String);
    utf8Injection = resultInjection decodeUTF8 encodeUTF8 where
    {
        decodeUTF8 :: [Word8] -> Result ListError String;
        decodeUTF8 os = evalStateT parse (os,0) where
        {
            getWord8 :: (Monad m) => StateT ([Word8],Int) m (Maybe Word8);
            getWord8 = StateT (\s@(bb,i) -> return (case bb of
            {
                b:bs -> (Just b,(bs,i+1));
                [] -> (Nothing,s);
            }));

            listError :: StateT (s,Int) (Result ListError) a;
            listError = StateT (\(_,i) -> FailureResult (MkListError i));

            parse :: StateT ([Word8],Int) (Result ListError) String;
            parse = do
            {
                mc <- parseChar;
                case mc of
                {
                    Just c -> do
                    {
                        s <- parse;
                        return (c:s);
                    };
                    _ -> return [];
                };
            };

            parseChar :: StateT ([Word8],Int) (Result ListError) (Maybe Char);
            parseChar = do
            {
                mb0 <- getWord8;
                case mb0 of
                {
                    Nothing -> return Nothing; -- Stream end
                    Just b0 -> do
                    {
                        if (testBit b0 7)
                         then if (testBit b0 6)
                          then if (testBit b0 5)
                           then if (testBit b0 4)
                            then if (testBit b0 3)
                             then listError
                             else do
                        {
                            let {w0 = fromIntegral (0x7 .&. b0);};
                            w1 <- get10Bits;
                            w2 <- get10Bits;
                            w3 <- get10Bits;
                            convertOut (
                                (shift w0 18) .|.
                                (shift w1 12) .|.
                                (shift w2 6) .|.
                                w3
                                );
                        }
                            else do
                        {
                            let {w0 = fromIntegral (0xF .&. b0);};
                            w1 <- get10Bits;
                            w2 <- get10Bits;
                            convertOut (
                                (shift w0 12) .|.
                                (shift w1 6) .|.
                                w2
                                );
                        }
                           else do
                        {
                            let {w0 = fromIntegral (0x1F .&. b0);};
                            w1 <- get10Bits;
                            convertOut (
                                (shift w0 6) .|. w1
                                );
                        }
                          else listError
                         else convertOut (
                             fromIntegral b0
                             );
                    }
                };
            } where
            {
                extract10Bits :: (Maybe Word8) -> StateT ([Word8],Int) (Result ListError) Word8;
                extract10Bits (Just w) | 0xC0 .&. w == 0x80 = return (0x3F .&. w);
                extract10Bits _ = listError;

                get10Bits :: StateT ([Word8],Int) (Result ListError) Word32;
                get10Bits = do
                {
                    mb <- getWord8;
                    b <- extract10Bits mb;
                    return (fromIntegral b);
                };

                convertOut :: Word32 -> StateT ([Word8],Int) (Result ListError) (Maybe Char);
                convertOut i|i < 0x110000 = return (Just (toEnum (fromIntegral i)));
                convertOut _ = listError;
            };
        };

        {--
        max Char: 0011 0000

        0000 0000-0000 007F   0xxxxxxx
        0000 0080-0000 07FF   110xxxxx 10xxxxxx
        0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
        0001 0000-001F FFFF   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
        historical only
        0020 0000-03FF FFFF   111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
        0400 0000-7FFF FFFF   1111110x 10xxxxxx ... 10xxxxxx
        --}

        encodeSingleUTF8 :: Char -> [Word8];
        encodeSingleUTF8 ch =
            if (c < 0x80) then
                [
                loByte c
                ]
            else if (c < 0x800) then
                [
                0xC0 .|. (shiftToByte 6),
                trailingByte 0
                ]
            else if (c < 0x10000) then
                [
                0xE0 .|. (shiftToByte 12),
                trailingByte 6,
                trailingByte 0
                ]
            else -- if (c < 0x200000) then
                [
                0xF0 .|. (shiftToByte 18),
                trailingByte 12,
                trailingByte 6,
                trailingByte 0
                ]
    {--
            else if (c < 0x4000000) then
                [
                0xF8 .|. (shiftToByte 24),
                trailingByte 18,
                trailingByte 12,
                trailingByte 6,
                trailingByte 0
                ]
            else
                [
                0xFC .|. (shiftToByte 30),
                trailingByte 24,
                trailingByte 18,
                trailingByte 12,
                trailingByte 6,
                trailingByte 0
                ]
    --}
                 where
        {
            c :: Word32;
            c = fromIntegral (fromEnum ch);

            loByte :: Word32 -> Word8;
            loByte = fromIntegral;

            shiftToByte :: Int -> Word8;
            shiftToByte i = loByte (shiftR c i);

            trailingByte :: Int -> Word8;
            trailingByte i = 0x80 .|. (0x3F .&. (shiftToByte i));
        };

        encodeUTF8 :: String -> [Word8];
        encodeUTF8 s = foldr prependOne [] s where
        {
            prependOne c bytes = (encodeSingleUTF8 c) ++ bytes;
        };
    };

}
