module Main
    ( main
    ) where

import Data.Time
import Documentation
import GitHash
import qualified Options.Applicative as O
import Pinafore
import Shapes
import System.Directory
import System.Environment.XDG.BaseDir
import Truth.Core
import Truth.UI.GTK

data Options
    = ShowVersionOption
    | PredefinedDocOption
    | InfixDocOption
    | DumpTableOption (Maybe FilePath)
    | RunFileOption UpdateTiming
                    Bool
                    (Maybe FilePath)
                    [FilePath]
    | RunInteractiveOption UpdateTiming
                           (Maybe FilePath)

optDataFlag :: O.Parser (Maybe FilePath)
optDataFlag = O.optional $ O.strOption $ O.long "data" <> O.metavar "PATH"

optSyncFlag :: O.Parser UpdateTiming
optSyncFlag =
    fmap
        (\f ->
             if f
                 then SynchronousUpdateTiming
                 else AsynchronousUpdateTiming) $
    O.switch $ O.long "sync"

optParser :: O.Parser Options
optParser =
    (O.flag' ShowVersionOption $ O.long "version" <> O.short 'v') <|>
    (RunFileOption <$> optSyncFlag <*> (O.switch $ O.long "no-run" <> O.short 'n') <*> optDataFlag <*>
     (O.many $ O.strArgument $ O.metavar "SCRIPT")) <|>
    ((O.flag' RunInteractiveOption $ O.long "interactive" <> O.short 'i') <*> optSyncFlag <*> optDataFlag) <|>
    (O.flag' PredefinedDocOption $ O.long "doc-predefined") <|>
    (O.flag' InfixDocOption $ O.long "doc-infix") <|>
    ((O.flag' DumpTableOption $ O.long "dump-table") <*> optDataFlag)

getDirPath :: MonadIO m => Maybe FilePath -> m FilePath
getDirPath mdirpath = do
    dirpath <-
        case mdirpath of
            Just dirpath -> return dirpath
            Nothing -> liftIO $ getUserDataDir "pinafore"
    liftIO $ createDirectoryIfMissing True dirpath
    return dirpath

pinaforeVersion :: String
pinaforeVersion = "0.1"

main :: IO ()
main = do
    options <- O.execParser (O.info optParser mempty)
    case options of
        ShowVersionOption -> let
            gi = $$tGitInfoCwd
            commitZonedTime :: ZonedTime
            commitZonedTime = parseTimeOrError True defaultTimeLocale "%a %b %-e %T %Y %z" (giCommitDate gi)
            commitTimeString :: String
            commitTimeString = formatTime defaultTimeLocale "%FT%TZ" $ zonedTimeToUTC commitZonedTime
            in putStrLn $
               "Pinafore version " <>
               pinaforeVersion <>
               " (" <>
               commitTimeString <>
               " " <>
               giHash gi <>
               ")" <>
               if giDirty gi
                   then "+"
                   else ""
        PredefinedDocOption -> printPredefinedBindings
        InfixDocOption -> printInfixOperatorTable
        DumpTableOption mdirpath -> do
            dirpath <- getDirPath mdirpath
            sqlitePinaforeDumpTable dirpath
        RunFileOption ut fNoRun mdirpath fpaths -> do
            dirpath <- getDirPath mdirpath
            truthMainGTK $ \MkTruthContext {..} -> do
                (toolkit, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
                context <- standardPinaforeContext ut dirpath toolkit
                for_ fpaths $ \fpath ->
                    liftIO $ do
                        ptext <- readFile fpath
                        action <-
                            ioRunInterpretResult $ let
                                ?pinafore = context
                                in pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
                        if fNoRun
                            then return ()
                            else action
                liftIO checkdone
        RunInteractiveOption ut mdirpath -> do
            dirpath <- getDirPath mdirpath
            truthMainGTK $ \MkTruthContext {..} -> do
                context <- standardPinaforeContext ut dirpath tcUIToolkit
                let
                    ?pinafore = context
                    in liftIO pinaforeInteract
                liftIO $ uitExit tcUIToolkit
