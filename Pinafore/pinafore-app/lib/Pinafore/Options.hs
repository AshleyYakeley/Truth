module Pinafore.Options
    ( RunOptions(..)
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

getApplicationOptions :: MonadIO m => RunOptions -> m (StorageModelOptions, ModuleOptions, ImportTranslatorOptions)
getApplicationOptions MkRunOptions {..} = do
    smoDataDir <- getPinaforeDir roDataDir
    sysIncludeDirs <- liftIO $ getSystemDataDirs "pinafore/lib"
    let
        smoCache = roCache
        moExtraLibrary = extraLibrary
        moModuleDirs = roIncludeDirs <> [smoDataDir </> "lib"] <> sysIncludeDirs
    return (MkStorageModelOptions {..}, MkModuleOptions {..}, importTranslatorOptions)
