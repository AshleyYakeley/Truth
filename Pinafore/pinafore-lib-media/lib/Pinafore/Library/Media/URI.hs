{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Library.Media.URI
    ( uriStuff
    )
where

import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64 qualified as Base64
import Data.Shim
import Network.URI qualified as URI
import Pinafore.API
import Shapes

import Pinafore.Library.Media.Media ()

instance AsLiteral URI.URI where
    literalCodec = asText . literalCodec

asText :: Codec Text URI.URI
asText = MkCodec (URI.parseURIReference . unpack) (pack . show)

-- URI
instance HasQGroundType '[] URI.URI where
    qGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily URI.URI)|]) "URI."

type MkURITypes :: [Type]
type MkURITypes = '[Text, Maybe URI.URIAuth, Text, Text, Text]

mkURITypes :: ListType QDocSignature MkURITypes
mkURITypes =
    ConsListType (mkValueDocSignature "scheme" "" Nothing)
        $ ConsListType (mkValueDocSignature "authority" "" $ Just Nothing)
        $ ConsListType (mkValueDocSignature "path" "" $ Just "")
        $ ConsListType (mkValueDocSignature "query" "" $ Just "")
        $ ConsListType (mkValueDocSignature "fragment" "" $ Just "")
        $ NilListType

mkURICodec :: Codec URI.URI (ListVProduct MkURITypes)
mkURICodec = let
    encode :: ListVProduct MkURITypes -> URI.URI
    encode vt = let
        (scheme, (uriAuthority, (path, (query, (fragment, ()))))) = listVProductToProduct vt
        uriScheme = unpack scheme
        uriPath = unpack path
        uriQuery = unpack query
        uriFragment = unpack fragment
        in URI.URI{..}
    decode :: URI.URI -> Maybe (ListVProduct MkURITypes)
    decode URI.URI{..} = do
        return
            $ listProductToVProduct
                (listTypeToVType mkURITypes)
                (pack uriScheme, (uriAuthority, (pack uriPath, (pack uriQuery, (pack uriFragment, ())))))
    in MkCodec{..}

-- URIAuth
instance HasQGroundType '[] URI.URIAuth where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily URI.URIAuth)|]) "Authority.URI."

type URIAuthTypes :: [Type]
type URIAuthTypes = '[Text, Text, Maybe Int]

uriAuthTypes :: ListType QDocSignature URIAuthTypes
uriAuthTypes =
    ConsListType (mkValueDocSignature "userInfo" "" $ Just "")
        $ ConsListType (mkValueDocSignature "host" "" Nothing)
        $ ConsListType (mkValueDocSignature "port" "" $ Just Nothing)
        $ NilListType

uriAuthCodec :: Codec URI.URIAuth (ListVProduct URIAuthTypes)
uriAuthCodec = let
    encode :: ListVProduct URIAuthTypes -> URI.URIAuth
    encode vt = let
        (userInfo, (host, (mPort, ()))) = listVProductToProduct vt
        uriUserInfo = unpack userInfo
        uriRegName = unpack host
        uriPort = maybe "" show mPort
        in URI.URIAuth{..}
    decode :: URI.URIAuth -> Maybe (ListVProduct URIAuthTypes)
    decode URI.URIAuth{..} = do
        mPort <- case uriPort of
            "" -> return Nothing
            _ -> fmap Just $ readMaybe uriPort
        return $ listProductToVProduct (listTypeToVType uriAuthTypes) (pack uriUserInfo, (pack uriRegName, (mPort, ())))
    in MkCodec{..}

type DataURITypes :: [Type]
type DataURITypes = '[Bool, Media, Text]

dataURITypes :: ListType QDocSignature DataURITypes
dataURITypes =
    ConsListType (mkValueDocSignature "base64" "" $ Just True)
        $ ConsListType (mkValueDocSignature "media" "" $ Nothing)
        $ ConsListType (mkValueDocSignature "fragment" "" $ Just "")
        $ NilListType

base64Encode :: StrictByteString -> Text
base64Encode = Base64.extractBase64 . Base64.encodeBase64

base64Decode :: String -> Maybe StrictByteString
base64Decode s = do
    ww <- for s encodeChar8
    mToMaybe $ Base64.decodeBase64Untyped $ pack ww

asciiEncode :: StrictByteString -> Text
asciiEncode = pack . URI.escapeURIString URI.isReserved . fmap decodeChar8 . unpack

asciiDecode :: String -> Maybe StrictByteString
asciiDecode s =
    fmap pack
        $ for (URI.unEscapeString s) encodeChar8

readIdentifier :: ReadPrec Text
readIdentifier = do
    s <- some $ rSatisfy URI.isReserved
    return $ pack s

readMedia :: ReadPrec (Media, Bool)
readMedia = do
    t <- readIdentifier
    rLiteral '/'
    s <- readIdentifier
    p <- many $ do
        rLiteral ';'
        k <- readIdentifier
        rLiteral '='
        v <- readIdentifier
        return (k, v)
    ob <- optional $ rLiterals ";base64"
    rLiteral ','
    remaining <- rWhole
    let
        mediaType = MkMediaType t s p
        isBase64 = isJust ob
    mediaContent <-
        maybe empty return
            $ if isBase64
                then base64Decode remaining
                else asciiDecode remaining
    return (MkMedia{..}, isBase64)

dataURICodec :: Codec URI.URI (ListVProduct DataURITypes)
dataURICodec = let
    encodeC :: ListVProduct DataURITypes -> URI.URI
    encodeC vt = let
        (isBase64, (MkMedia{..}, (fragment, ()))) = listVProductToProduct vt
        uriScheme = "data"
        uriAuthority = Nothing
        uriPath =
            unpack
                $ encode textMediaTypeCodec mediaType
                <> (if isBase64 then ";base64" else "")
                <> ","
                <> (if isBase64 then base64Encode else asciiEncode) mediaContent
        uriQuery = ""
        uriFragment = unpack fragment
        in URI.URI{..}
    decodeC :: URI.URI -> Maybe (ListVProduct DataURITypes)
    decodeC URI.URI{..} = do
        guard $ uriScheme == "data"
        guard $ not $ isJust uriAuthority
        (media, isBase64) <- runReadPrec readMedia uriPath
        return
            $ listProductToVProduct
                (listTypeToVType dataURITypes)
                (isBase64, (media, (pack uriFragment, ())))
    in MkCodec decodeC encodeC

uriStuff :: LibraryStuff
uriStuff =
    headingBDS "URI" ""
        $ [ typeBDS
                "URI"
                "A Uniform Resource Identifier."
                (qSomeGroundType @_ @URI.URI)
                [ recordConsBDS "Mk" "" mkURITypes mkURICodec
                , recordConsBDS "Data" "`data` URI (RFC 2397)" dataURITypes dataURICodec
                ]
          , hasSubtypeRelationBDS @URI.URI @Text Verify "" $ functionToShim "asText.encode" $ encode asText
          , namespaceBDS
                "URI"
                [ valBDS "relativeTo" "Interpret the first URI relative to the second." URI.relativeTo
                , valBDS "relativeFrom" "Create a relative URI that represents the first URI relative to the second." URI.relativeFrom
                , typeBDS
                    "Authority"
                    "URI authority."
                    (qSomeGroundType @_ @URI.URIAuth)
                    [recordConsBDS "Mk" "" uriAuthTypes uriAuthCodec]
                ]
          ]
