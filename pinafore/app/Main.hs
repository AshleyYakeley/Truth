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
import Version

getDirPath :: MonadIO m => Maybe FilePath -> m FilePath
getDirPath mdirpath = do
    dirpath <-
        case mdirpath of
            Just dirpath -> return dirpath
            Nothing -> liftIO $ getUserDataDir "pinafore"
    liftIO $ createDirectoryIfMissing True dirpath
    return dirpath

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        PredefinedDocOption -> printPredefinedBindings
        InfixDocOption -> printInfixOperatorTable
        DumpTableOption mdirpath -> do
            dirpath <- getDirPath mdirpath
            sqlitePinaforeDumpTable dirpath
        RunFileOption ut fNoRun mdirpath fpaths -> do
            dirpath <- getDirPath mdirpath
            runFiles ut fNoRun dirpath fpaths
        RunInteractiveOption ut mdirpath -> do
            dirpath <- getDirPath mdirpath
            runInteractive ut dirpath
