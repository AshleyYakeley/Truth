{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Library.Media.URI
    ( uriStuff
    )
where

import Data.Shim
import Network.URI (URI (..), URIAuth (..), parseURIReference, relativeFrom, relativeTo)
import Pinafore.API
import Shapes

instance AsLiteral URI where
    literalCodec = asText . literalCodec

asText :: Codec Text URI
asText = MkCodec (parseURIReference . unpack) (pack . show)

-- URI
instance HasQGroundType '[] URI where
    qGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily URI)|]) "URI."

type URITypes :: [Type]
type URITypes = '[Text, Maybe URIAuth, Text, Text, Text]

uriTypes :: ListType QDocSignature URITypes
uriTypes =
    ConsListType (mkValueDocSignature "scheme" "" Nothing)
        $ ConsListType (mkValueDocSignature "authority" "" $ Just Nothing)
        $ ConsListType (mkValueDocSignature "path" "" $ Just "")
        $ ConsListType (mkValueDocSignature "query" "" $ Just "")
        $ ConsListType (mkValueDocSignature "fragment" "" $ Just "")
        $ NilListType

uriCodec :: Codec URI (ListVProduct URITypes)
uriCodec = let
    encode :: ListVProduct URITypes -> URI
    encode vt = let
        (scheme, (uriAuthority, (path, (query, (fragment, ()))))) = listVProductToProduct vt
        uriScheme = unpack scheme
        uriPath = unpack path
        uriQuery = unpack query
        uriFragment = unpack fragment
        in URI{..}
    decode :: URI -> Maybe (ListVProduct URITypes)
    decode URI{..} = do
        return
            $ listProductToVProduct
                (listTypeToVType uriTypes)
                (pack uriScheme, (uriAuthority, (pack uriPath, (pack uriQuery, (pack uriFragment, ())))))
    in MkCodec{..}

-- URIAuth
instance HasQGroundType '[] URIAuth where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily URIAuth)|]) "Authority.URI."

type URIAuthTypes :: [Type]
type URIAuthTypes = '[Text, Text, Maybe Int]

uriAuthTypes :: ListType QDocSignature URIAuthTypes
uriAuthTypes =
    ConsListType (mkValueDocSignature "userInfo" "" $ Just "")
        $ ConsListType (mkValueDocSignature "host" "" Nothing)
        $ ConsListType (mkValueDocSignature "port" "" $ Just Nothing)
        $ NilListType

uriAuthCodec :: Codec URIAuth (ListVProduct URIAuthTypes)
uriAuthCodec = let
    encode :: ListVProduct URIAuthTypes -> URIAuth
    encode vt = let
        (userInfo, (host, (mPort, ()))) = listVProductToProduct vt
        uriUserInfo = unpack userInfo
        uriRegName = unpack host
        uriPort = maybe "" show mPort
        in URIAuth{..}
    decode :: URIAuth -> Maybe (ListVProduct URIAuthTypes)
    decode URIAuth{..} = do
        mPort <- case uriPort of
            "" -> return Nothing
            _ -> fmap Just $ readMaybe uriPort
        return $ listProductToVProduct (listTypeToVType uriAuthTypes) (pack uriUserInfo, (pack uriRegName, (mPort, ())))
    in MkCodec{..}

uriStuff :: LibraryStuff
uriStuff =
    headingBDS "URI" ""
        $ [ typeBDS
                "URI"
                "A Uniform Resource Identifier."
                (qSomeGroundType @_ @URI)
                [recordConsBDS "Mk" "" uriTypes uriCodec]
          , hasSubtypeRelationBDS @URI @Text Verify "" $ functionToShim "asText.encode" $ encode asText
          , namespaceBDS
                "URI"
                [ valBDS "relativeTo" "Interpret the first URI relative to the second." relativeTo
                , valBDS "relativeFrom" "Create a relative URI that represents the first URI relative to the second." relativeFrom
                , typeBDS
                    "Authority"
                    "URI authority."
                    (qSomeGroundType @_ @URIAuth)
                    [recordConsBDS "Mk" "" uriAuthTypes uriAuthCodec]
                ]
          ]
