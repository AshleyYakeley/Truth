module Pinafore.Libs where

import Pinafore
import Pinafore.Language.Library.GNOME
import Pinafore.Language.Library.Media
import Pinafore.WebAPI
import Shapes
import System.Directory
import System.Environment.XDG.BaseDir

getPinaforeDir :: Maybe FilePath -> IO FilePath
getPinaforeDir mdirpath = do
    pinaforedir <-
        case mdirpath of
            Just pinaforedir -> return pinaforedir
            Nothing -> getUserDataDir "pinafore"
    createDirectoryIfMissing True pinaforedir
    return pinaforedir

extraLibrary :: [LibraryModule ()]
extraLibrary = mediaLibrary <> gnomeLibrary

importers :: [Importer]
importers = webAPIImporters
