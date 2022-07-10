module Pinafore.Libs where

import Pinafore
import Pinafore.Language.Library.GTK
import Pinafore.Language.Library.Media
import Shapes
import System.Directory
import System.Environment.XDG.BaseDir

getPinaforeDir :: MonadIO m => Maybe FilePath -> m FilePath
getPinaforeDir mdirpath = do
    pinaforedir <-
        case mdirpath of
            Just pinaforedir -> return pinaforedir
            Nothing -> liftIO $ getUserDataDir "pinafore"
    liftIO $ createDirectoryIfMissing True pinaforedir
    return pinaforedir

extraLibrary :: [LibraryModule]
extraLibrary = mediaLibrary <> gtkLibrary
