module Pinafore.Base.Anchor
    ( IsHash(..)
    , Anchor
    , checkAnchor
    , anchorCodec
    , hashToAnchor
    , codeAnchor
    , byteStringToAnchor
    , anchorToByteString
    ) where

import BLAKE3
import Control.DeepSeq
import Data.ByteArray (convert)
import qualified Data.Serialize as Serialize (Serialize(..), encode)
import Shapes

newtype Anchor =
    MkAnchor StrictByteString
    deriving (Eq, Ord, NFData, Hashable)

class IsHash h where
    hashSize :: Int
    hashByteStrings :: [StrictByteString] -> h

instance IsHash Anchor where
    hashSize = fromIntegral $ typeValue @_ @NaturalType @DEFAULT_DIGEST_LEN -- 256 bits = 64 hex chars = 32 bytes = 4 Word64s
    hashByteStrings f = MkAnchor $ convert $ hash @DEFAULT_DIGEST_LEN f

checkAnchor :: String -> Anchor -> Anchor
checkAnchor s anchor@(MkAnchor bs) =
    if olength bs == hashSize @Anchor
        then anchor
        else error $ s <> ": broken anchor (" <> show (olength bs) <> ")"

anchorCodec ::
       forall m. MonadFail m
    => Codec' m StrictByteString Anchor
anchorCodec = let
    decode :: StrictByteString -> m Anchor
    decode bs =
        if olength bs == hashSize @Anchor
            then return $ MkAnchor bs
            else fail $ "deserialize: bad anchor (" <> show (olength bs) <> ")"
    encode :: Anchor -> StrictByteString
    encode (checkAnchor "encode" -> MkAnchor bs) = bs
    in MkCodec {..}

randomN ::
       forall t g. (Random t, RandomGen g)
    => Int
    -> g
    -> ([t], g)
randomN 0 g = ([], g)
randomN n g = let
    (a, g') = random g
    (as, g'') = randomN (pred n) g'
    in (a : as, g'')

instance Random Anchor where
    randomR _ = random
    random g0 = let
        (ww, g) = randomN @Word64 (div (hashSize @Anchor) 8) g0
        in (MkAnchor $ mconcat $ fmap Serialize.encode ww, g)

showHexChar :: Word8 -> Char
showHexChar w
    | w < 10 = toEnum $ (fromEnum '0') + (fromEnum w)
showHexChar w = toEnum $ (fromEnum 'A') + (fromEnum $ w - 10)

showWord8Hex :: Word8 -> [Char]
showWord8Hex w = [showHexChar $ div w 16, showHexChar $ mod w 16]

groupList :: Int -> [a] -> [[a]]
groupList _ [] = []
groupList n aa = let
    (p, q) = splitAt n aa
    in p : groupList n q

showBSHex :: StrictByteString -> [Char]
showBSHex bs = mconcat $ fmap showWord8Hex $ otoList bs

instance Show Anchor where
    show (MkAnchor bs) = '!' : (intercalate "-" $ groupList 8 $ showBSHex bs)

instance Serialize Anchor where
    put = Serialize.put . encodeM anchorCodec
    get = do
        bs <- Serialize.get
        decode anchorCodec bs

hashToAnchor :: (forall r. (forall t. Serialize t => t -> r) -> [r]) -> Anchor
hashToAnchor f = hashByteStrings $ f Serialize.encode

codeAnchor :: Text -> Anchor
codeAnchor text = hashToAnchor $ \call -> [call @Text "anchor:", call text]

byteStringToAnchor :: StrictByteString -> Anchor
byteStringToAnchor bs =
    MkAnchor $ let
        len = olength bs
        in if len <= 31
               then cons (fromIntegral len) bs <> replicate (31 - len) 0
               else case hashToAnchor $ \call -> [call @Text "literal:", call bs] of
                        MkAnchor bs' -> cons 255 $ drop 1 bs'

anchorToByteString :: Anchor -> Maybe StrictByteString
anchorToByteString (MkAnchor bs) = let
    len = fromIntegral $ headEx bs
    in if len <= 31
           then Just $ take len $ tailEx bs
           else Nothing
