module Pinafore.WebAPI.HTTP
    ( callHTTP
    ) where

import Data.Aeson as Aeson
import Network.HTTP.Simple
import Network.HTTP.Types (statusCode)
import Shapes

callHTTP :: Text -> Text -> Value -> IO Value
callHTTP op uri obj = do
    plainRequest <- parseRequest $ unpack uri
    let
        request :: Request
        request = setRequestBodyJSON obj $ setRequestMethod (encodeUtf8 op) plainRequest
    response <- httpJSON request
    case statusCode $ getResponseStatus response of
        200 -> return $ getResponseBody response
        201 -> return $ getResponseBody response
        204 -> return Null
        i -> fail $ "failed with HTTP status " <> show i
