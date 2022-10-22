module Pinafore.Options
    ( RunOptions(..)
    , getStorageModelOptions
    ) where

import Pinafore
import Pinafore.Libs
import Shapes
import System.FilePath

data RunOptions = MkRunOptions
    { roCache :: Bool
    , roIncludeDirs :: [FilePath]
    , roDataDir :: Maybe FilePath
    } deriving (Eq, Show)

stdIncludeDirs :: FilePath -> [FilePath]
stdIncludeDirs pinaforedir = [pinaforedir </> "lib", "/usr/local/share/pinafore/lib", "/usr/share/pinafore/lib"]

getStorageModelOptions :: MonadIO m => RunOptions -> m (StorageModelOptions, ModuleOptions)
getStorageModelOptions MkRunOptions {..} = do
    smoDataDir <- getPinaforeDir roDataDir
    let
        smoCache = roCache
        moExtraLibrary = extraLibrary
        moModuleDirs = roIncludeDirs <> stdIncludeDirs smoDataDir
    return (MkStorageModelOptions {..}, MkModuleOptions {..})
