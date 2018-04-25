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
                (Maybe FilePath)
                [FilePath]

optDataFlag :: O.Parser (Maybe FilePath)
optDataFlag = O.optional $ O.strOption (O.long "data")

optParser :: O.Parser Options
optParser =
    (O.flag' ExprDocOption $ O.long "doc") <|> (O.flag' DumpTableOption $ O.long "dump-table") <*> optDataFlag <|>
    RunOption <$> (O.switch $ O.long "interactive" <> O.short 'i') <*> optDataFlag <*> (O.many $ O.strArgument mempty)

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
    truthMain $ \args createWindow -> do
        options <- liftIO $ O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        case options of
            ExprDocOption ->
                liftIO $ do
                    for_ (predefinedDoc @PinaforeEdit) $ \(name, desc) ->
                        putStrLn $ (show name) ++ " :: " ++ unpack desc
                    putStrLn $ "<file> :: " ++ unpack filePinaforeType
            DumpTableOption mdirpath -> do
                dirpath <- getDirPath mdirpath
                liftIO $ sqlitePinaforeDumpTable dirpath
            RunOption fInteract mdirpath fpaths -> do
                dirpath <- getDirPath mdirpath
                context <- sqlitePinaforeContext dirpath createWindow
                liftIO $
                    case fpaths of
                        [] -> do
                            isterm <- hIsTerminalDevice stdin
                            if isterm || fInteract
                                then pinaforeInteract context
                                else do
                                    ptext <- getContents
                                    pinaforeRunFile context "<stdin>" $ decodeUtf8 $ toStrict ptext
                        _ -> do
                            for_ fpaths $ \fpath -> do
                                ptext <- readFile fpath
                                pinaforeRunFile context fpath $ decodeUtf8 $ toStrict ptext
                            if fInteract
                                then pinaforeInteract context
                                else return ()
