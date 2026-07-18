module Main
    ( main
    )
where

import Pinafore.Main
import Shapes
import System.Environment
import System.Exit

import Options
import Pinafore.Options
import Pinafore.Version
import Run

excHandler :: SomeException -> IO a
excHandler exc =
    case fromException exc of
        Just ec -> exitWith ec
        Nothing -> do
            progname <- getProgName
            hPutStrLn stderr $ progname <> ": " <> show exc
            exitFailure

main :: IO ()
main =
    handleExc excHandler
        $ getOptions
        >>= \case
            ShowVersionOption -> printVersion
            DumpTableOption mdirpath -> do
                pinaforedir <- ensurePinaforeDir mdirpath
                sqliteQDumpTable pinaforedir
            RunFileOption ropts fNoRun fscript -> do
                copts <- getModuleOptions ropts
                runFiles copts fNoRun [fscript]
            RunInteractiveOption ropts -> do
                copts <- getModuleOptions ropts
                runInteractive copts
