module Main
    ( main
    ) where

import Documentation
import Options
import Pinafore
import Run
import Shapes
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath
import Version

getPinaforeDir :: MonadIO m => Maybe FilePath -> m FilePath
getPinaforeDir mdirpath = do
    pinaforedir <-
        case mdirpath of
            Just pinaforedir -> return pinaforedir
            Nothing -> liftIO $ getUserDataDir "pinafore"
    liftIO $ createDirectoryIfMissing True pinaforedir
    return pinaforedir

stdIncludeDirs :: FilePath -> [FilePath]
stdIncludeDirs pinaforedir = [pinaforedir </> "lib", "/usr/local/share/pinafore/lib", "/usr/share/pinafore/lib"]

getContextOptions :: MonadIO m => RunOptions -> m ContextOptions
getContextOptions MkRunOptions {..} = do
    coDataDir <- getPinaforeDir roDataDir
    let
        coCache = roCache
        coModuleDirs = roIncludeDirs <> stdIncludeDirs coDataDir
    return MkContextOptions {..}

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        PredefinedDocOption -> printPredefinedBindings
        InfixDocOption -> printInfixOperatorTable
        DumpTableOption mdirpath -> do
            pinaforedir <- getPinaforeDir mdirpath
            sqlitePinaforeDumpTable pinaforedir
        RunFileOption ropts fNoRun fscript -> do
            copts <- getContextOptions ropts
            runFiles copts fNoRun [fscript]
        RunInteractiveOption ropts -> do
            copts <- getContextOptions ropts
            runInteractive copts
