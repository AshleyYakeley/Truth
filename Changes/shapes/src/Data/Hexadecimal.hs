module Data.Hexadecimal where

import Data.Filterable
import Shapes.Import

class AsHexadecimal t where
    toHexadecimal :: t -> String
    fromStrictHexadecimal :: String -> Maybe t

fromLooseHexadecimal :: AsHexadecimal t => String -> Maybe t
fromLooseHexadecimal s = fromStrictHexadecimal $ filter isAlphaNum s

instance AsHexadecimal Word8 where
    toHexadecimal = let
        toHexChar :: Word8 -> Char
        toHexChar w
            | w < 10 = toEnum $ (fromEnum '0') + (fromEnum w)
        toHexChar w = toEnum $ (fromEnum 'A') + (fromEnum $ w - 10)
        in \w -> [toHexChar $ div w 16, toHexChar $ mod w 16]
    fromStrictHexadecimal [hc, lc] = do
        let
            fromHexChar :: Char -> Maybe Word8
            fromHexChar (fromEnum -> c) =
                if c >= (fromEnum '0') && c <= (fromEnum '9')
                    then Just $ toEnum $ c - (fromEnum '0')
                    else
                        if c >= (fromEnum 'A') && c <= (fromEnum 'F')
                            then Just $ toEnum $ c - (fromEnum 'A') + 10
                            else
                                if c >= (fromEnum 'a') && c <= (fromEnum 'f')
                                    then Just $ toEnum $ c - (fromEnum 'a') + 10
                                    else Nothing
        h <- fromHexChar hc
        l <- fromHexChar lc
        return $ h * 16 + l
    fromStrictHexadecimal _ = Nothing

toUppercaseHexadecimal :: AsHexadecimal a => a -> String
toUppercaseHexadecimal = toHexadecimal

toLowercaseHexadecimal :: AsHexadecimal a => a -> String
toLowercaseHexadecimal = toLower . toHexadecimal

instance AsHexadecimal [Word8] where
    toHexadecimal ww = concatmap toHexadecimal ww
    fromStrictHexadecimal [] = return []
    fromStrictHexadecimal (a : b : r) = do
        w1 <- fromStrictHexadecimal [a, b]
        wr <- fromStrictHexadecimal r
        return $ w1 : wr
    fromStrictHexadecimal [_] = Nothing

instance AsHexadecimal StrictByteString where
    toHexadecimal bs = toHexadecimal $ unpack bs
    fromStrictHexadecimal s = fmap pack $ fromStrictHexadecimal s
