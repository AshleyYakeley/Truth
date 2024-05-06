module Pinafore.Options
    ( RunOptions(..)
    , getModelOptions
    , getApplicationOptions
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

getModelOptions :: RunOptions -> IO ModuleOptions
getModelOptions MkRunOptions {..} = do
    smoDataDir <- getPinaforeDir roDataDir
    sysIncludeDirs <- getSystemDataDirs "pinafore/lib"
    let
        moExtraLibrary = extraLibrary
        moModuleDirs = roIncludeDirs <> [smoDataDir </> "lib"] <> sysIncludeDirs
    return MkModuleOptions {..}

getApplicationOptions :: RunOptions -> IO (StorageModelOptions, ModuleOptions)
getApplicationOptions MkRunOptions {..} = do
    smoDataDir <- ensurePinaforeDir roDataDir
    sysIncludeDirs <- getSystemDataDirs "pinafore/lib"
    let
        smoCache = roCache
        moExtraLibrary = extraLibrary
        moModuleDirs = roIncludeDirs <> [smoDataDir </> "lib"] <> sysIncludeDirs
    return (MkStorageModelOptions {..}, MkModuleOptions {..})
