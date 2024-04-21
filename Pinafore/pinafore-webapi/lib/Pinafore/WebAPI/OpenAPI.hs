module Pinafore.WebAPI.OpenAPI
    ( openAPIImporter
    ) where

import Data.Aeson
import Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
import Pinafore.Language
import Pinafore.Language.API
import Pinafore.WebAPI.Fetch
import Shapes

importOpenAPI :: Text -> ResultT Text IO (LibraryStuff ())
importOpenAPI t = do
    bs <- fetch t
    jsonval <-
        case eitherDecode bs of
            Left err -> liftInner $ FailureResult $ "invalid JSON: " <> pack err
            Right jsonval -> return jsonval
    root :: OpenApi <-
        case fromJSON jsonval of
            Error err -> liftInner $ FailureResult $ pack err
            Success val -> return val
    return $ valBDS "schema" "" $ fmap showText $ InsOrd.toList $ _componentsSchemas $ _openApiComponents root

openAPIImporter :: Importer
openAPIImporter = MkImporter "openapi" importOpenAPI
