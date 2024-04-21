module Pinafore.WebAPI.Fetch where

import Network.URI
import Shapes

fetch :: Text -> IO LazyByteString
fetch t =
    case parseURIReference $ unpack t of
        Just uri ->
            case uriScheme uri of
                "file" -> readFile $ uriPath uri
                s -> fail $ "URI schema " <> show s <> " not supported"
        Nothing -> fail $ show (unpack t) <> " is not a URI"
