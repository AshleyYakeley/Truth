module Pinafore.Language.Convert.DataLiteral
    ( DecodeMedia(..)
    , DataLiteral
    , dlData
    , bytesToDataLiteralM
    , bytesToDataLiteral
    , dataLiteralMediaCodec
    ) where

import Changes.World.Media.Type
import Import

class DecodeMedia t where
    dmContentType :: MediaType
    dmMatchContentType :: MediaType -> Bool
    dmDecode :: StrictByteString -> Maybe t

data DataLiteral t = MkDataLiteral
    { dlBytes :: StrictByteString
    , dlData :: t
    }

instance Eq (DataLiteral t) where
    a == b = dlBytes a == dlBytes b

bytesToDataLiteralM :: DecodeMedia t => StrictByteString -> Maybe (DataLiteral t)
bytesToDataLiteralM bs = do
    t <- dmDecode bs
    return $ MkDataLiteral bs t

bytesToDataLiteral :: DecodeMedia t => StrictByteString -> DataLiteral t
bytesToDataLiteral bs = MkDataLiteral bs $ fromMaybe (error "bad literal decode") $ dmDecode bs

dataLiteralMediaCodec ::
       forall t. DecodeMedia t
    => Codec Media (DataLiteral t)
dataLiteralMediaCodec = let
    decode :: Media -> Maybe (DataLiteral t)
    decode (MkMedia mt bs)
        | dmMatchContentType @t mt = bytesToDataLiteralM bs
    decode _ = Nothing
    encode :: DataLiteral t -> Media
    encode dl = MkMedia (dmContentType @t) (dlBytes dl)
    in MkCodec {..}

instance DecodeMedia t => AsLiteral (DataLiteral t) where
    literalCodec = dataLiteralMediaCodec . literalMediaCodec
