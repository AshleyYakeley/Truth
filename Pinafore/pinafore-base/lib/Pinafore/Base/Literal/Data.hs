module Pinafore.Base.Literal.Data
    ( DecodeLiteral(..)
    , DataLiteral
    , dlData
    , bytesToDataLiteralM
    , bytesToDataLiteral
    , DecodeMedia(..)
    , dataLiteralMediaCodec
    ) where

import Changes.World.Media.Type
import Pinafore.Base.Literal.Literal
import Pinafore.Base.Literal.Type
import Pinafore.Base.Media
import Shapes

class DecodeLiteral t where
    dmLiteralType :: LiteralType
    default dmLiteralType :: DecodeMedia t => LiteralType
    dmLiteralType = generalLiteralType $ dmMediaType @t
    dmDecode :: StrictByteString -> Maybe t

data DataLiteral t = MkDataLiteral
    { dlBytes :: StrictByteString
    , dlData :: t
    }

instance Eq (DataLiteral t) where
    a == b = dlBytes a == dlBytes b

bytesToDataLiteralM :: DecodeLiteral t => StrictByteString -> Maybe (DataLiteral t)
bytesToDataLiteralM bs = do
    t <- dmDecode bs
    return $ MkDataLiteral bs t

bytesToDataLiteral :: DecodeLiteral t => StrictByteString -> DataLiteral t
bytesToDataLiteral bs = fromMaybe (error "bad literal decode") $ bytesToDataLiteralM bs

dataLiteralCodec :: DecodeLiteral t => Codec StrictByteString (DataLiteral t)
dataLiteralCodec = MkCodec bytesToDataLiteralM dlBytes

instance DecodeLiteral t => AsLiteral (DataLiteral t)

instance DecodeLiteral t => AsTypedLiteral (DataLiteral t) where
    literalType = dmLiteralType @t
    literalContentSerializer = codecSerializer dataLiteralCodec

class DecodeLiteral t => DecodeMedia t where
    dmMediaType :: MediaType
    dmMatchContentType :: MediaType -> Bool

dataLiteralMediaCodec ::
       forall t. DecodeMedia t
    => Codec Media (DataLiteral t)
dataLiteralMediaCodec = let
    decode :: Media -> Maybe (DataLiteral t)
    decode (MkMedia mt bs)
        | dmMatchContentType @t mt = bytesToDataLiteralM bs
    decode _ = Nothing
    encode :: DataLiteral t -> Media
    encode dl = MkMedia (dmMediaType @t) (dlBytes dl)
    in MkCodec {..}
