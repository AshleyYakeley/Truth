{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.Media.Media
    ( Media(..)
    , DecodeMedia(..)
    , dataLiteralMediaCodec
    , mediaSpecificText
    , mediaEntityLibSection
    ) where

import Changes.World.Media.Type
import Pinafore.API
import Shapes

data Media = MkMedia
    { mediaType :: MediaType
    , mediaContent :: StrictByteString
    } deriving (Eq)

instance AsLiteral Media

instance AsTypedLiteral Media where
    literalType = mediaLiteralType

instance HasSerializer Media where
    greedySerializer = let
        toMedia :: (MediaType, StrictByteString) -> Media
        toMedia (t, b) = MkMedia t b
        fromMedia :: Media -> (MediaType, StrictByteString)
        fromMedia (MkMedia t b) = (t, b)
        in invmap toMedia fromMedia $ sProduct stoppingSerializer greedySerializer
    stoppingSerializer = let
        toMedia :: (MediaType, StrictByteString) -> Media
        toMedia (t, b) = MkMedia t b
        fromMedia :: Media -> (MediaType, StrictByteString)
        fromMedia (MkMedia t b) = (t, b)
        in invmap toMedia fromMedia $ sProduct stoppingSerializer stoppingSerializer

-- MediaType
mediaTypeGroundType :: QGroundType '[] MediaType
mediaTypeGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily MediaType)|]) "Type.Media."

instance HasQGroundType '[] MediaType where
    qGroundType = mediaTypeGroundType

-- Media
mediaGroundType :: QGroundType '[] Media
mediaGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Media)|]) "Media"

instance HasQGroundType '[] Media where
    qGroundType = mediaGroundType

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

mediaBlob :: Codec Media StrictByteString
mediaBlob = let
    blobMediaType :: MediaType
    blobMediaType = MkMediaType ApplicationMediaType "octet-stream" []
    encode = MkMedia blobMediaType
    decode (MkMedia _ bs) = Just bs
    in MkCodec {..}

decodeUTF8 :: StrictByteString -> Maybe Text
decodeUTF8 = eitherRight . decodeUtf8'

-- not quite correct, but good enough
decodeASCII :: StrictByteString -> Maybe Text
decodeASCII = decodeUTF8

getMediaTextEncoding :: MediaType -> Maybe (StrictByteString -> Maybe Text)
getMediaTextEncoding (MkMediaType TextMediaType _ p) =
    case fmap toLower $ lookup "charset" p of
        Nothing -> Just decodeASCII
        Just "utf-8" -> Just decodeUTF8
        Just "iso-8859-1" -> Just $ Just . decodeLatin1
        Just "us-ascii" -> Just decodeASCII
        Just _ -> Nothing
getMediaTextEncoding (MkMediaType ApplicationMediaType "xml" _) = Just decodeUTF8
getMediaTextEncoding (MkMediaType ApplicationMediaType s _)
    | isJust $ endsWith "+xml" $ unpack s = Just decodeUTF8
getMediaTextEncoding (MkMediaType ApplicationMediaType "html" _) = Just decodeUTF8
getMediaTextEncoding _ = Nothing

mediaSpecificText :: MediaType -> (MediaType -> Bool) -> Codec Media Text
mediaSpecificText (MkMediaType pt ps pp) f = let
    pp' =
        pp <>
        case pt of
            TextMediaType -> [("charset", "utf-8")]
            _ -> []
    pmt :: MediaType
    pmt = MkMediaType pt ps pp'
    ee :: Text -> Media
    ee text = MkMedia pmt $ encode utf8Codec text
    dd :: Media -> Maybe Text
    dd (MkMedia mt b) = do
        altIf $ f mt
        encoding <- getMediaTextEncoding mt
        encoding b
    in MkCodec dd ee

mediaText :: Codec Media Text
mediaText = mediaSpecificText (MkMediaType TextMediaType "plain" []) (\_ -> True)

mediaEntityLibSection :: LibraryStuff
mediaEntityLibSection =
    headingBDS
        "Media"
        ""
        [ namespaceBDS
              "Media"
              [ typeBDS
                    "Type"
                    "RFC 6838 media type."
                    (MkSomeGroundType mediaTypeGroundType)
                    [ valPatBDS "Mk" "Type, subtype, parameters" MkMediaType $
                      PureFunction $ pure $ \(MkMediaType t s p) -> (t, (s, (p, ())))
                    ]
              , literalSubtypeRelationEntry @MediaType
              ]
        , typeBDS
              "Media"
              "A blob and an RFC 6838 media type that interprets it."
              (MkSomeGroundType mediaGroundType)
              [valPatBDS "Mk" "Type and Blob" MkMedia $ PureFunction $ pure $ \(MkMedia t b) -> (t, (b, ()))]
        , literalSubtypeRelationEntry @Media
        , namespaceBDS "Blob" [valBDS "asMedia" "Decodes any media." $ codecToPrism mediaBlob]
        , namespaceBDS "Text" [valBDS "asMedia" "Decodes any text-like media." $ codecToPrism mediaText]
        ]
