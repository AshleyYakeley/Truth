module Pinafore.Base.Anchor
    ( IsHash (..)
    , Anchor
    , checkAnchor
    , anchorCodec
    , hashToAnchor
    , codeAnchor
    , byteStringToAnchor
    , anchorToByteString
    )
where

import BLAKE3 qualified
import Control.DeepSeq
import Data.ByteArray (convert)
import Shapes

newtype Anchor
    = MkAnchor StrictByteString
    deriving newtype (Eq, Ord, NFData, Hashable)

class IsHash h where
    hashSize :: Int
    hashByteStrings :: [StrictByteString] -> h

instance IsHash Anchor where
    hashSize = fromIntegral $ typeValue @_ @NaturalType @BLAKE3.DEFAULT_DIGEST_LEN -- 256 bits = 64 hex chars = 32 bytes = 4 Word64s
    hashByteStrings f =
        MkAnchor $ convert $ BLAKE3.hash @BLAKE3.DEFAULT_DIGEST_LEN @(BLAKE3.Digest BLAKE3.DEFAULT_DIGEST_LEN) Nothing f

anchorSize :: Int
anchorSize = hashSize @Anchor

checkAnchor :: String -> Anchor -> Anchor
checkAnchor s anchor@(MkAnchor bs) =
    if olength bs == anchorSize
        then anchor
        else error $ s <> ": broken anchor (" <> show (olength bs) <> ")"

anchorCodec ::
    forall m.
    MonadFail m =>
    Codec' m StrictByteString Anchor
anchorCodec = let
    decode :: StrictByteString -> m Anchor
    decode bs =
        if olength bs == anchorSize
            then return $ MkAnchor bs
            else fail $ "deserialize: bad anchor (" <> show (olength bs) <> ")"
    encode :: Anchor -> StrictByteString
    encode (checkAnchor "encode" -> MkAnchor bs) = bs
    in MkCodec{..}

randomN ::
    forall t g.
    (Random t, RandomGen g) =>
    Int ->
    g ->
    ([t], g)
randomN 0 g = ([], g)
randomN n g = let
    (a, g') = random g
    (as, g'') = randomN (pred n) g'
    in (a : as, g'')

instance Random Anchor where
    randomR _ = random
    random g0 = let
        (ww, g) = randomN @Word8 anchorSize g0
        in (MkAnchor $ pack ww, g)

groupList :: Int -> [a] -> [[a]]
groupList _ [] = []
groupList n aa = let
    (p, q) = splitAt n aa
    in p : groupList n q

instance Show Anchor where
    show (MkAnchor bs) = '!' : (intercalate "-" $ groupList 8 $ toHexadecimal bs)

instance HasSerializer Anchor where
    stoppingSerializer = isoCoerce $ fixedByteStringSerializer anchorSize

hashToAnchor :: (forall r. (forall t. HasSerializer t => t -> r) -> [r]) -> Anchor
hashToAnchor f = hashByteStrings $ f $ encode serializeStrictCodec

codeAnchor :: Text -> Anchor
codeAnchor text = hashToAnchor $ \call -> [call @Text "anchor:", call text]

byteStringToAnchor :: StrictByteString -> Anchor
byteStringToAnchor bs =
    MkAnchor $ let
        maxlength = pred anchorSize
        len = olength bs
        in if len <= maxlength
            then cons (fromIntegral len) bs <> replicate (maxlength - len) 0
            else case hashToAnchor $ \call -> [call @Text "literal:", call bs] of
                MkAnchor bs' -> cons 255 $ drop 1 bs'

anchorToByteString :: Anchor -> Maybe StrictByteString
anchorToByteString (MkAnchor bs) = let
    maxlength = pred anchorSize
    len = fromIntegral $ headEx bs
    in if len <= maxlength
        then Just $ take len $ tailEx bs
        else Nothing
