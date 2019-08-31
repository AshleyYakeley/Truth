module Main
    ( main
    ) where

import Data.Time
import GitHash
import qualified Options.Applicative as O
import Pinafore
import Pinafore.Language.Documentation
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

escapeMarkdown :: String -> String
escapeMarkdown s = let
    badchars :: String
    badchars = "+-*>\\"
    escapeChar :: Char -> String
    escapeChar c =
        if elem c badchars
            then ['\\', c]
            else [c]
    in mconcat $ fmap escapeChar s

showDefEntry :: Int -> DefDoc -> IO ()
showDefEntry _ MkDefDoc {..} = do
    putStrLn $
        "**`" ++
        show docName ++
        "`** :: `" ++
        unpack docValueType ++
        "`" <>
        (if docIsPattern
             then " (also pattern)"
             else "") <>
        "  "
    if docDescription == ""
        then return ()
        else putStrLn $ escapeMarkdown $ unpack docDescription
    putStrLn ""

showDefTitle :: Int -> Text -> IO ()
showDefTitle level title = putStrLn $ replicate level '#' ++ " " ++ unpack title

showDefDesc :: Int -> Text -> IO ()
showDefDesc _ "" = return ()
showDefDesc _ desc = do
    putStrLn $ unpack desc
    putStrLn ""

printInfixOperatorTable :: IO ()
printInfixOperatorTable = do
    let names = filter nameIsInfix $ fmap docName $ toList $ predefinedDoc @PinaforeUpdate
    putStrLn "| [n] | (A x B) x C | A x (B x C) | A x B only |"
    putStrLn "| --- | --- | --- | --- |"
    for_ [10,9 .. 0] $ \level -> do
        putStr $ show level
        for_ [AssocLeft, AssocRight, AssocNone] $ \assc -> do
            putStr " |"
            let
                fixity = MkFixity assc level
                mnames = filter (\n -> operatorFixity n == fixity) names
            for_ mnames $ \n -> putStr $ " `" <> show n <> "`"
        putStrLn ""

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
        PredefinedDocOption -> runDocTree showDefTitle showDefDesc showDefEntry 1 $ predefinedDoc @PinaforeUpdate
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
