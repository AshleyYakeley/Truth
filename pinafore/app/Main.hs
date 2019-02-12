module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Pinafore
import Shapes
import System.Directory
import System.Environment.XDG.BaseDir
import Truth.UI.GTK

data Options
    = ExprDocOption
    | DumpTableOption (Maybe FilePath)
    | RunOption Bool
                Bool
                (Maybe FilePath)
                [FilePath]

optDataFlag :: O.Parser (Maybe FilePath)
optDataFlag = O.optional $ O.strOption $ O.long "data" <> O.metavar "PATH"

optParser :: O.Parser Options
optParser =
    (RunOption <$> (O.switch $ O.long "interactive" <> O.short 'i') <*> (O.switch $ O.long "no-run" <> O.short 'n') <*>
     optDataFlag <*>
     (O.many $ O.strArgument $ O.metavar "SCRIPT")) <|>
    (O.flag' ExprDocOption $ O.long "doc") <|>
    ((O.flag' DumpTableOption $ O.long "dump-table") <*> optDataFlag)

getDirPath :: MonadIO m => Maybe FilePath -> m FilePath
getDirPath mdirpath = do
    dirpath <-
        case mdirpath of
            Just dirpath -> return dirpath
            Nothing -> liftIO $ getUserDataDir "pinafore"
    liftIO $ createDirectoryIfMissing True dirpath
    return dirpath

showDefEntry :: Int -> DefDoc -> IO ()
showDefEntry _ MkDefDoc {..} = let
    badchars :: String
    badchars = "+-*>\\"
    escapeChar :: Char -> String
    escapeChar c =
        if elem c badchars
            then ['\\', c]
            else [c]
    escapeMarkdown :: String -> String
    escapeMarkdown s = mconcat $ fmap escapeChar s
    in do
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

main :: IO ()
main =
    truthMain $ \args createWindow -> do
        options <- liftIO $ O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        case options of
            ExprDocOption ->
                liftIO $ do runDocTree showDefTitle showDefDesc showDefEntry 1 $ predefinedDoc @PinaforeEdit
                    -- putMarkdown "<file>" (unpack filePinaforeType) "a script file passed to pinafore"
            DumpTableOption mdirpath -> do
                dirpath <- getDirPath mdirpath
                liftIO $ sqlitePinaforeDumpTable dirpath
            RunOption fInteract fNoRun mdirpath fpaths -> do
                dirpath <- getDirPath mdirpath
                context <- sqlitePinaforeContext dirpath createWindow
                let
                    ?pinafore = context
                    in liftIO $
                       case fpaths of
                           [] -> do
                               isterm <- hIsTerminalDevice stdin
                               if isterm || fInteract
                                   then pinaforeInteract
                                   else do
                                       ptext <- getContents
                                       action <- pinaforeInterpretFile "<stdin>" $ decodeUtf8 $ toStrict ptext
                                       if fNoRun
                                           then return ()
                                           else action
                           _ -> do
                               for_ fpaths $ \fpath -> do
                                   ptext <- readFile fpath
                                   action <- pinaforeInterpretFile fpath $ decodeUtf8 $ toStrict ptext
                                   if fNoRun
                                       then return ()
                                       else action
                               if fInteract
                                   then pinaforeInteract
                                   else return ()
