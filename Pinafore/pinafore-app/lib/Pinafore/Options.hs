module Pinafore.Options
    ( RunOptions(..)
    , getStorageModelOptions
    ) where

import Pinafore
import Pinafore.Libs
import Shapes
import System.Environment.XDG.BaseDir
import System.FilePath

data RunOptions = MkRunOptions
    { roCache :: Bool
    , roIncludeDirs :: [FilePath]
    , roDataDir :: Maybe FilePath
    } deriving (Eq, Show)

getStorageModelOptions :: RunOptions -> IO (StorageModelOptions, ModuleOptions)
getStorageModelOptions MkRunOptions {..} = do
    smoDataDir <- getPinaforeDir roDataDir
    sysIncludeDirs <- getSystemDataDirs "pinafore/lib"
    let
        smoCache = roCache
        moExtraLibrary = extraLibrary
        moModuleDirs = roIncludeDirs <> [smoDataDir </> "lib"] <> sysIncludeDirs
    return (MkStorageModelOptions {..}, MkModuleOptions {..})
