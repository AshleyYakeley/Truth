module Data.Char8 where

import Data.ByteString.Internal

import Shapes.Import

-- | Latin-1
decodeChar8 :: Word8 -> Char
decodeChar8 = w2c

-- | Latin-1
encodeChar8 :: Char -> Maybe Word8
encodeChar8 c =
    if c < '\x0100'
        then Just $ c2w c
        else Nothing

-- | US-ASCII
decodeChar7 :: Word8 -> Maybe Char
decodeChar7 w =
    if w < 0x80
        then Just $ w2c w
        else Nothing

-- | US-ASCII
encodeChar7 :: Char -> Maybe Word8
encodeChar7 c =
    if c < '\x0080'
        then Just $ c2w c
        else Nothing
