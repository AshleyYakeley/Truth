module Pinafore.WebAPI.HTTP
    ( HTTPEndpoint(..),HTTPFunction(..),HTTPRequest(..),callHTTP
    ) where

import Data.Aeson as Aeson
import Network.HTTP.Simple
import Network.HTTP.Types (statusCode)
import Shapes

-- covers http: and https:
data HTTPEndpoint = MkHTTPEndpoint {
    heSecure :: Bool,
    heHost :: Text,
    hePort :: Word16,
    hePath :: Text
}

data HTTPFunction = MkHTTPFunction {
    hfOperation :: Text,
    hfEndpoint :: HTTPEndpoint,
    hfPathTemplate :: Text
}

-- covers http: and https:
data HTTPRequest = MkHTTPRequest {
    hrFunction :: HTTPFunction,
    hrPathVars :: [(Text, Text)],
    hrQuery :: [(Text, Maybe Text)],
    hrBody :: Maybe Value
}

substTemplate :: Text -> [(Text, Text)] -> Text
substTemplate = foo

httpRequestToRequest :: HTTPRequest -> Request
httpRequestToRequest MkHTTPRequest{..} = let
    MkHTTPFunction {..} = hrFunction
    MkHTTPEndpoint {..} = hfEndpoint
    path = hePath <> substTemplate hfPathTemplate hrPathVars
    request0 :: Request
    request0 =
        setRequestMethod (encodeUtf8 hfOperation) $
        setRequestQueryString (fmap (\(k,v) -> (encodeUtf8 k,fmap encodeUtf8 v)) hrQuery) $
        setRequestPath (encodeUtf8 path) $
        setRequestPort (fromIntegral hePort) $
        setRequestHost (encodeUtf8 heHost) $
        setRequestSecure heSecure $ defaultRequest
    request :: Request
    request = case hrBody of
        Just body -> setRequestBodyJSON body request0
        Nothing -> request0
    in request

callHTTP :: HTTPRequest -> IO Value
callHTTP hr mbody = do
    response <- httpJSON $ httpRequestToRequest hr
    case statusCode $ getResponseStatus response of
        200 -> return $ getResponseBody response
        201 -> return $ getResponseBody response
        204 -> return Null
        i -> fail $ "failed with HTTP status " <> show i
