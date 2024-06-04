module Pinafore.Libs where

import Pinafore.API
import Pinafore.Library.GNOME
import Pinafore.Library.Media
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
