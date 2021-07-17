module Pinafore.Options
    ( RunOptions(..)
    , getContextOptions
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

getContextOptions :: MonadIO m => RunOptions -> m (ContextOptions, ModuleOptions)
getContextOptions MkRunOptions {..} = do
    coDataDir <- getPinaforeDir roDataDir
    let
        coCache = roCache
        moExtraLibrary = extraLibrary
        moModuleDirs = roIncludeDirs <> stdIncludeDirs coDataDir
    return (MkContextOptions {..}, MkModuleOptions {..})
