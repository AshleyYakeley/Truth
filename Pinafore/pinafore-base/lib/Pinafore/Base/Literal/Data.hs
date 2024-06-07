module Pinafore.Base.Literal.Data
    ( DecodeLiteral(..)
    , DataLiteral
    , dlBytes
    , dlData
    , bytesToDataLiteralM
    , bytesToDataLiteral
    ) where

import Pinafore.Base.Literal.Literal
import Pinafore.Base.Literal.Type
import Shapes

class DecodeLiteral t where
    dmLiteralType :: LiteralType
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
