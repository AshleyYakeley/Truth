module Main
    ( main
    ) where

import Options
import Pinafore
import Pinafore.Libs
import Pinafore.Options
import Pinafore.Version
import Run
import Shapes

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
