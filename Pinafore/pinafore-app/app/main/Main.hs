module Main
    ( main
    )
where

import Pinafore.Main
import Shapes

import Options
import Pinafore.Options
import Pinafore.Version
import Run

main :: IO ()
main =
    getOptions >>= \case
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
