module Pinafore.WebAPI
    ( webAPIImporters
    ) where

import Pinafore.Language
import Pinafore.WebAPI.OpenAPI

webAPIImporters :: [Importer]
webAPIImporters = [openAPIImporter]
