module Pinafore.Options
    ( RunOptions(..)
    , getModuleOptions
    ) where

import Pinafore.Libs
import Pinafore.Main
import Shapes
import System.Environment.XDG.BaseDir
import System.FilePath

data RunOptions = MkRunOptions
    { roCache :: Bool
    , roIncludeDirs :: [FilePath]
    , roDataDir :: Maybe FilePath
    } deriving (Eq, Show)

getModuleOptions :: RunOptions -> IO ModuleOptions
getModuleOptions MkRunOptions {..} = do
    setPinaforeDir roDataDir
    dataDir <- getPinaforeDir
    sysIncludeDirs <- getSystemDataDirs "pinafore/lib"
    let
        moLibraryModules = appLibrary
        moModuleDirs = roIncludeDirs <> [dataDir </> "lib"] <> sysIncludeDirs
    return MkModuleOptions {..}
