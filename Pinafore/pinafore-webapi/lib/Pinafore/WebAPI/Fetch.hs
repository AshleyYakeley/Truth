module Pinafore.WebAPI.Fetch where

import Network.URI
import Pinafore.Text
import Shapes

fetch :: Text -> ResultT Text IO LazyByteString
fetch t =
    case parseURIReference $ unpack t of
        Just uri ->
            case uriScheme uri of
                "file:" -> liftIO $ readFile $ uriPath uri
                s -> liftInner $ throwExc $ "URI schema " <> showText s <> " not supported"
        Nothing -> liftInner $ throwExc $ showText t <> " is not a URI"
