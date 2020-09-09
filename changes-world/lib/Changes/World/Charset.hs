module Changes.World.Charset where

import Changes.Core
import Shapes

utf8Codec :: ReasonCodec [Word8] String
utf8Codec = MkCodec decodeUTF8 encodeUTF8
  where
    decodeUTF8 :: [Word8] -> Result Text String
    decodeUTF8 os = evalStateT parse (os, 0)
      where
        getWord8 :: (Monad m) => StateT ([Word8], Int) m (Maybe Word8)
        getWord8 =
            StateT
                (\s@(bb, i) ->
                     return
                         (case bb of
                              b:bs -> (Just b, (bs, i + 1))
                              [] -> (Nothing, s)))
        decodeError :: StateT (s, Int) (Result Text) a
        decodeError = StateT (\(_, i) -> fail $ "decode error at byte " ++ show i)
        parse :: StateT ([Word8], Int) (Result Text) String
        parse = do
            mc <- parseChar
            case mc of
                Just c -> do
                    s <- parse
                    return (c : s)
                _ -> return []
        parseChar :: StateT ([Word8], Int) (Result Text) (Maybe Char)
        parseChar = do
            mb0 <- getWord8
            case mb0 of
                Nothing -> return Nothing -- Stream end
                Just b0 -> do
                    if (testBit b0 7)
                        then if (testBit b0 6)
                                 then if (testBit b0 5)
                                          then if (testBit b0 4)
                                                   then if (testBit b0 3)
                                                            then decodeError
                                                            else do
                                                                let w0 = fromIntegral (0x7 .&. b0)
                                                                w1 <- get10Bits
                                                                w2 <- get10Bits
                                                                w3 <- get10Bits
                                                                convertOut
                                                                    ((shift w0 18) .|. (shift w1 12) .|. (shift w2 6) .|.
                                                                     w3)
                                                   else do
                                                       let w0 = fromIntegral (0xF .&. b0)
                                                       w1 <- get10Bits
                                                       w2 <- get10Bits
                                                       convertOut ((shift w0 12) .|. (shift w1 6) .|. w2)
                                          else do
                                              let w0 = fromIntegral (0x1F .&. b0)
                                              w1 <- get10Bits
                                              convertOut ((shift w0 6) .|. w1)
                                 else decodeError
                        else convertOut (fromIntegral b0)
          where
            extract10Bits :: (Maybe Word8) -> StateT ([Word8], Int) (Result Text) Word8
            extract10Bits (Just w)
                | 0xC0 .&. w == 0x80 = return (0x3F .&. w)
            extract10Bits _ = decodeError
            get10Bits :: StateT ([Word8], Int) (Result Text) Word32
            get10Bits = do
                mb <- getWord8
                b <- extract10Bits mb
                return (fromIntegral b)
            convertOut :: Word32 -> StateT ([Word8], Int) (Result Text) (Maybe Char)
            convertOut i
                | i < 0x110000 = return (Just (toEnum (fromIntegral i)))
            convertOut _ = decodeError
    encodeSingleUTF8 :: Char -> [Word8]
    encodeSingleUTF8 ch =
        if (c < 0x80)
            then [loByte c]
            else if (c < 0x800)
                     then [0xC0 .|. (shiftToByte 6), trailingByte 0]
                     else if (c < 0x10000)
                              then [0xE0 .|. (shiftToByte 12), trailingByte 6, trailingByte 0]
                              else [0xF0 .|. (shiftToByte 18), trailingByte 12, trailingByte 6, trailingByte 0]
      where
        c :: Word32
        c = fromIntegral (fromEnum ch)
        loByte :: Word32 -> Word8
        loByte = fromIntegral
        shiftToByte :: Int -> Word8
        shiftToByte i = loByte (shiftR c i)
        trailingByte :: Int -> Word8
        trailingByte i = 0x80 .|. (0x3F .&. (shiftToByte i))
    encodeUTF8 :: String -> [Word8]
    encodeUTF8 s = foldr prependOne [] s
      where
        prependOne c bytes = (encodeSingleUTF8 c) ++ bytes

type CharsetKnowledge = String -> Maybe (Codec [Word8] String)
