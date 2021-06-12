module Main
    ( main
    ) where

import Options
import Pinafore
import Pinafore.Libs
import Pinafore.Version
import Run
import Shapes
import System.FilePath

stdIncludeDirs :: FilePath -> [FilePath]
stdIncludeDirs pinaforedir = [pinaforedir </> "lib", "/usr/local/share/pinafore/lib", "/usr/share/pinafore/lib"]

getContextOptions :: MonadIO m => RunOptions -> m ContextOptions
getContextOptions MkRunOptions {..} = do
    coDataDir <- getPinaforeDir roDataDir
    let
        coCache = roCache
        coExtraLibrary = extraLibrary
        coModuleDirs = roIncludeDirs <> stdIncludeDirs coDataDir
    return MkContextOptions {..}

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        DumpTableOption mdirpath -> do
            pinaforedir <- getPinaforeDir mdirpath
            sqlitePinaforeDumpTable pinaforedir
        RunFileOption ropts fNoRun fscript -> do
            copts <- getContextOptions ropts
            runFiles copts fNoRun [fscript]
        RunInteractiveOption ropts -> do
            copts <- getContextOptions ropts
            runInteractive copts
