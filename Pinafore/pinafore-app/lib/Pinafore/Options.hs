module Pinafore.Options
    ( RunOptions(..)
    , getModuleOptions
    ) where

import qualified Paths_pinafore_lib_script
import Pinafore.Libs
import Pinafore.Main
import Shapes
import System.Environment.XDG.BaseDir
import System.FilePath

data RunOptions = MkRunOptions
    { roIncludeDirs :: [FilePath]
    , roDataDir :: Maybe FilePath
    } deriving (Eq, Show)

getModuleOptions :: RunOptions -> IO ModuleOptions
getModuleOptions MkRunOptions {..} = do
    setPinaforeDir roDataDir
    dataDir <- getPinaforeDir
    sysIncludeDirs <- getSystemDataDirs "pinafore/lib"
    scriptLibDir <- Paths_pinafore_lib_script.getDataDir
    let
        moLibraryModules = appLibrary
        moModuleDirs = roIncludeDirs <> [dataDir </> "lib", scriptLibDir] <> sysIncludeDirs
    return MkModuleOptions {..}
