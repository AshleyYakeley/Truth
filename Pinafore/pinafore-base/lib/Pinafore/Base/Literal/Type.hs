module Pinafore.Base.Literal.Type
    ( LiteralType
    , runLiteralType
    , generalLiteralType
    , blobLiteralType
    , textLiteralType
    , booleanLiteralType
    , orderingLiteralType
    , rationalLiteralType
    , doubleLiteralType
    , dayLiteralType
    , timeOfDayLiteralType
    , localTimeLiteralType
    , timeLiteralType
    , durationLiteralType
    , colourLiteralType
    , mediaTypeLiteralType
    , mediaLiteralType
    )
where

import Shapes

newtype LiteralType = MkLiteralType
    { runLiteralType :: Serializer 'Stops ()
    }

generalLiteralType :: MediaType -> LiteralType
generalLiteralType t = MkLiteralType $ sLiteralBytes [0x6D] ***> sExact t stoppingSerializer -- [m]

tagLiteralType :: [Word8] -> LiteralType
tagLiteralType tag = MkLiteralType $ sLiteralBytes tag

blobLiteralType :: LiteralType
blobLiteralType = tagLiteralType [0x61] -- [a]

textLiteralType :: LiteralType
textLiteralType = tagLiteralType [0x74] -- [t]

booleanLiteralType :: LiteralType
booleanLiteralType = tagLiteralType [0x62] -- [b]

orderingLiteralType :: LiteralType
orderingLiteralType = tagLiteralType [0x6F] -- [o]

rationalLiteralType :: LiteralType
rationalLiteralType = tagLiteralType [0x72] -- [r]

doubleLiteralType :: LiteralType
doubleLiteralType = tagLiteralType [0x64] -- [d]

dayLiteralType :: LiteralType
dayLiteralType = tagLiteralType [0x54, 0x64] -- [Td]

timeOfDayLiteralType :: LiteralType
timeOfDayLiteralType = tagLiteralType [0x54, 0x6F] -- [To]

localTimeLiteralType :: LiteralType
localTimeLiteralType = tagLiteralType [0x54, 0x6C] -- [Tl]

timeLiteralType :: LiteralType
timeLiteralType = tagLiteralType [0x54, 0x75] -- [Tu]

durationLiteralType :: LiteralType
durationLiteralType = tagLiteralType [0x54, 0x6E] -- [Tn]

colourLiteralType :: LiteralType
colourLiteralType = tagLiteralType [0x63] -- [c]

mediaTypeLiteralType :: LiteralType
mediaTypeLiteralType = tagLiteralType [0x79] -- [y]

mediaLiteralType :: LiteralType
mediaLiteralType = tagLiteralType [0x4D] -- [M]
