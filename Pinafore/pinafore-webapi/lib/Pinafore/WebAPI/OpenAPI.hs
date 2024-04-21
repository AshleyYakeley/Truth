module Pinafore.WebAPI.OpenAPI
    ( openAPIImporter
    ) where

import Data.Aeson
import Data.OpenApi
import Pinafore.Language
import Pinafore.Language.API
import Pinafore.WebAPI.Fetch
import Shapes

importOpenAPI :: Text -> ResultT Text IO (LibraryStuff ())
importOpenAPI t = do
    bs <- lift $ fetch t
    jsonval <-
        case eitherDecode bs of
            Left err -> liftInner $ FailureResult $ "invalid JSON: " <> pack err
            Right jsonval -> return jsonval
    sc :: Schema <-
        case fromJSON jsonval of
            Error err -> liftInner $ FailureResult $ pack err
            Success val -> return val
    return $ valBDS "schema" "" $ (pack $ show sc :: Text)

openAPIImporter :: Importer
openAPIImporter = MkImporter "openapi" importOpenAPI
