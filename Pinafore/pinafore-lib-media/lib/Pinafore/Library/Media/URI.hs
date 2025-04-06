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
asText = MkCodec (URI.parseURIReference . unpack) (\uri -> pack $ URI.uriToString id uri "")

-- URI
instance HasQGroundType '[] URI.URI where
    qGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily URI.URI)|]) "URI."

type MkURITypes :: [Type]
type MkURITypes = '[Maybe Text, Maybe URI.URIAuth, Text, Maybe Text, Maybe Text]

mkURITypes :: ListType QDocSignature MkURITypes
mkURITypes =
    ConsListType (mkValueDocSignature "scheme" "The scheme (trailing ':' omitted)." $ Just Nothing)
        $ ConsListType (mkValueDocSignature "authority" "" $ Just Nothing)
        $ ConsListType (mkValueDocSignature "path" "" $ Just "")
        $ ConsListType (mkValueDocSignature "query" "The query, if it exists (leading '?' omitted)." $ Just Nothing)
        $ ConsListType (mkValueDocSignature "fragment" "The fragment, if it exists (leading '#' omitted)." $ Just Nothing)
        $ NilListType

stripFirst :: Char -> String -> String
stripFirst _ "" = ""
stripFirst x (c : cc) | x == c = cc
stripFirst _ cc = cc

stripLast :: Char -> String -> String
stripLast _ "" = ""
stripLast x [c] | x == c = ""
stripLast x (c : cc) = c : stripLast x cc

encodeModify :: (String -> String) -> Maybe Text -> String
encodeModify f = maybe "" $ \t -> f $ unpack t

decodeModify :: (String -> String) -> String -> Maybe Text
decodeModify f = \case
    "" -> Nothing
    s -> Just $ pack $ f s

mkURICodec :: Codec URI.URI (ListVProduct MkURITypes)
mkURICodec = let
    encode :: ListVProduct MkURITypes -> URI.URI
    encode vt = let
        (scheme, (uriAuthority, (path, (query, (fragment, ()))))) = listVProductToProduct vt
        uriScheme = encodeModify (\s -> s <> ":") scheme
        uriPath = unpack path
        uriQuery = encodeModify (\s -> '?' : s) query
        uriFragment = encodeModify (\s -> '#' : s) fragment
        in URI.URI{..}
    decode :: URI.URI -> Maybe (ListVProduct MkURITypes)
    decode URI.URI{..} = return $ let
        mURIScheme = decodeModify (stripLast ':') uriScheme
        mURIQuery = decodeModify (stripFirst '?') uriQuery
        mURIFragment = decodeModify (stripFirst '#') uriFragment
        in listProductToVProduct
            (listTypeToVType mkURITypes)
            (mURIScheme, (uriAuthority, (pack uriPath, (mURIQuery, (mURIFragment, ())))))
    in MkCodec{..}

-- URIAuth
instance HasQGroundType '[] URI.URIAuth where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily URI.URIAuth)|]) "Authority.URI."

type URIAuthTypes :: [Type]
type URIAuthTypes = '[Maybe Text, Text, Maybe Int]

uriAuthTypes :: ListType QDocSignature URIAuthTypes
uriAuthTypes =
    ConsListType (mkValueDocSignature "userinfo" "The user info (trailing '@' omitted)." $ Just Nothing)
        $ ConsListType (mkValueDocSignature "host" "" Nothing)
        $ ConsListType (mkValueDocSignature "port" "" $ Just Nothing)
        $ NilListType

uriAuthCodec :: Codec URI.URIAuth (ListVProduct URIAuthTypes)
uriAuthCodec = let
    encode :: ListVProduct URIAuthTypes -> URI.URIAuth
    encode vt = let
        (mUserInfo, (host, (mPort, ()))) = listVProductToProduct vt
        uriUserInfo = maybe "" (\p -> show p <> "@") mUserInfo
        uriRegName = unpack host
        uriPort = maybe "" (\p -> ':' : show p) mPort
        in URI.URIAuth{..}
    decode :: URI.URIAuth -> Maybe (ListVProduct URIAuthTypes)
    decode URI.URIAuth{..} = return $ let
        mUserInfo = case uriUserInfo of
            "" -> Nothing
            _ -> Just $ pack $ stripLast '@' uriUserInfo
        mPort = readMaybe $ stripFirst ':' uriPort
        in listProductToVProduct (listTypeToVType uriAuthTypes) (mUserInfo, (pack uriRegName, (mPort, ())))
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
asciiEncode = pack . URI.escapeURIString (\c -> URI.isReserved c || URI.isUnreserved c) . fmap decodeChar8 . unpack

asciiDecode :: String -> Maybe StrictByteString
asciiDecode s =
    fmap pack
        $ for (URI.unEscapeString s) encodeChar8

defaultTextType :: MediaType
defaultTextType = MkMediaType "text" "plain" [("charset", "US-ASCII")]

readMediaType :: ReadPrec MediaType
readMediaType = do
    mType <- rMaybe readPrec
    return $ fromMaybe defaultTextType mType

readMedia :: ReadPrec (Media, Bool)
readMedia = do
    mediaType <- readMediaType
    isBase64 <- fmap isJust $ rMaybe $ rLiterals ";base64"
    rLiteral ','
    remaining <- rWhole
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
        uriScheme = "data:"
        uriAuthority = Nothing
        uriPath =
            unpack
                $ (if mediaType == defaultTextType then "" else showMediaType mediaType)
                <> (if isBase64 then ";base64" else "")
                <> ","
                <> (if isBase64 then base64Encode else asciiEncode) mediaContent
        uriQuery = ""
        uriFragment = unpack fragment
        in URI.URI{..}
    decodeC :: URI.URI -> Maybe (ListVProduct DataURITypes)
    decodeC URI.URI{..} = do
        guard $ uriScheme == "data:"
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
                , valBDS "isAbsolute" "Whether this is an absolute (rather than a relative) URI.." URI.uriIsAbsolute
                , typeBDS
                    "Authority"
                    "URI authority."
                    (qSomeGroundType @_ @URI.URIAuth)
                    [recordConsBDS "Mk" "" uriAuthTypes uriAuthCodec]
                ]
          ]
