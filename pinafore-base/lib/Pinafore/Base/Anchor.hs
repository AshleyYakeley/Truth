module Pinafore.Base.Anchor
    ( Anchor
    , anchorCodec
    , hashToAnchor
    , codeAnchor
    ) where

import Control.DeepSeq
import Crypto.Hash
import Data.ByteArray (convert)
import qualified Data.Serialize as Serialize (Serialize(..), encode)
import Shapes

newtype Anchor =
    MkAnchor StrictByteString -- 256 bits = 64 hex chars = 32 bytes = 4 Word64s
    deriving (Eq, Ord, NFData, Hashable)

anchorCodec ::
       forall m. MonadFail m
    => Codec' m StrictByteString Anchor
anchorCodec = let
    decode :: StrictByteString -> m Anchor
    decode bs =
        case olength bs of
            32 -> return $ MkAnchor bs
            _ -> fail "deserialize: bad anchor"
    encode :: Anchor -> StrictByteString
    encode (MkAnchor bs) =
        case olength bs of
            32 -> bs
            n -> error $ "encode: broken anchor (" <> show n <> ")"
    in MkCodec {..}

mkAnchor :: Word64 -> Word64 -> Word64 -> Word64 -> Anchor
mkAnchor a0 a1 a2 a3 = MkAnchor $ mconcat $ fmap Serialize.encode [a0, a1, a2, a3]

instance Random Anchor where
    randomR _ = random
    random g0 = let
        (a0, g1) = random g0
        (a1, g2) = random g1
        (a2, g3) = random g2
        (a3, g4) = random g3
        in (mkAnchor a0 a1 a2 a3, g4)

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
hashToAnchor f = MkAnchor $ convert $ hashFinalize $ hashUpdates (hashInit @SHA3_256) $ f Serialize.encode

codeAnchor :: Text -> Anchor
codeAnchor text = hashToAnchor $ \call -> [call @Text "anchor:", call text]
