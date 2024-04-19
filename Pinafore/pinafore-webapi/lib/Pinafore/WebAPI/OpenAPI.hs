module Pinafore.WebAPI.OpenAPI
    ( openAPIImporter
    ) where

import Pinafore.Language
import Pinafore.WebAPI.Fetch ()
import Shapes

importOpenAPI :: Text -> ResultT Text IO (LibraryContents ())
importOpenAPI _t = liftInner $ FailureResult "NYI"

openAPIImporter :: Importer
openAPIImporter = MkImporter "openapi" importOpenAPI
