module Pinafore.Options
    ( RunOptions (..)
    , getLibraryContext
    )
where

import Pinafore.Main
import Shapes
import System.Environment.XDG.BaseDir
import System.FilePath

import Pinafore.Packages

data RunOptions = MkRunOptions
    { roIncludeDirs :: [FilePath]
    , roDataDir :: Maybe FilePath
    }
    deriving stock (Eq, Show)

getLibraryContext :: RunOptions -> IO LibraryContext
getLibraryContext MkRunOptions{..} = do
    setPinaforeDir roDataDir
    dataDir <- getPinaforeDir
    sysIncludeDirs <- getSystemDataDirs "pinafore/lib"
    let
        dirsPackages = includeDirsPackages $ roIncludeDirs <> [dataDir </> "lib"] <> sysIncludeDirs
    return $ createLibraryContext $ dirsPackages <> appPackages
