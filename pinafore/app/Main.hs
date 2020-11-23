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

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        PredefinedDocOption -> printPredefinedBindings
        InfixDocOption -> printInfixOperatorTable
        DumpTableOption mdirpath -> do
            pinaforedir <- getPinaforeDir mdirpath
            sqlitePinaforeDumpTable pinaforedir
        RunFileOption fNoRun reqIncludeDirs mdirpath fscript -> do
            pinaforedir <- getPinaforeDir mdirpath
            runFiles fNoRun (reqIncludeDirs <> stdIncludeDirs pinaforedir) pinaforedir [fscript]
        RunInteractiveOption reqIncludeDirs mdirpath -> do
            pinaforedir <- getPinaforeDir mdirpath
            runInteractive (reqIncludeDirs <> stdIncludeDirs pinaforedir) pinaforedir
