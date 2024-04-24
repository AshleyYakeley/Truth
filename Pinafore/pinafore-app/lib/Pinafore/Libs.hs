module Pinafore.Libs where

import Pinafore
import Pinafore.Language.Library.GNOME
import Pinafore.Language.Library.Media
import Pinafore.WebAPI
import Shapes
import System.Directory
import System.Environment.XDG.BaseDir

getPinaforeDir :: Maybe FilePath -> IO FilePath
getPinaforeDir (Just pinaforedir) = return pinaforedir
getPinaforeDir Nothing = getUserDataDir "pinafore"

ensurePinaforeDir :: Maybe FilePath -> IO FilePath
ensurePinaforeDir mdirpath = do
    pinaforedir <- getPinaforeDir mdirpath
    createDirectoryIfMissing True pinaforedir
    return pinaforedir

extraLibrary :: [LibraryModule ()]
extraLibrary = mediaLibrary <> gnomeLibrary

importers :: [Importer]
importers = webAPIImporters
