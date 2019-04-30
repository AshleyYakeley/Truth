module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Pinafore
import Pinafore.Language.Documentation
import Shapes
import System.Directory
import System.Environment.XDG.BaseDir
import Truth.Core
import Truth.UI.GTK

data Options
    = PredefinedDocOption
    | InfixDocOption
    | DumpTableOption (Maybe FilePath)
    | RunFileOption Bool
                    Bool
                    (Maybe FilePath)
                    [FilePath]
    | RunInteractiveOption Bool
                           (Maybe FilePath)

optDataFlag :: O.Parser (Maybe FilePath)
optDataFlag = O.optional $ O.strOption $ O.long "data" <> O.metavar "PATH"

optParser :: O.Parser Options
optParser =
    (RunFileOption <$> (O.switch $ O.long "sync") <*> (O.switch $ O.long "no-run" <> O.short 'n') <*> optDataFlag <*>
     (O.many $ O.strArgument $ O.metavar "SCRIPT")) <|>
    ((O.flag' RunInteractiveOption $ O.long "interactive" <> O.short 'i') <*> (O.switch $ O.long "sync") <*> optDataFlag) <|>
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
    let names = filter nameIsInfix $ fmap docName $ toList $ predefinedDoc @PinaforeEdit
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

main :: IO ()
main = do
    options <- O.execParser (O.info optParser mempty)
    case options of
        PredefinedDocOption -> runDocTree showDefTitle showDefDesc showDefEntry 1 $ predefinedDoc @PinaforeEdit
        InfixDocOption -> printInfixOperatorTable
        DumpTableOption mdirpath -> do
            dirpath <- getDirPath mdirpath
            sqlitePinaforeDumpTable dirpath
        RunFileOption fSync fNoRun mdirpath fpaths -> do
            dirpath <- getDirPath mdirpath
            truthMainGTK $ \MkTruthContext {..} -> do
                (toolkit, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
                lifeCycleClose $ putStrLn "Closer"
                context <- sqlitePinaforeContext (not fSync) dirpath toolkit
                let
                    runText fpath ptext = do
                        action <-
                            ioRunInterpretResult $ let
                                ?pinafore = context
                                in pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
                        if fNoRun
                            then return ()
                            else action
                liftIO $
                    case fpaths of
                        [] -> do
                            ptext <- getContents
                            runText "<stdin>" ptext
                        _ -> do
                            for_ fpaths $ \fpath -> do
                                ptext <- readFile fpath
                                runText fpath ptext
                liftIO checkdone
        RunInteractiveOption fSync mdirpath -> do
            dirpath <- getDirPath mdirpath
            truthMainGTK $ \MkTruthContext {..} -> do
                context <- sqlitePinaforeContext (not fSync) dirpath tcUIToolkit
                let
                    ?pinafore = context
                    in liftIO pinaforeInteract
                liftIO $ uitExit tcUIToolkit
