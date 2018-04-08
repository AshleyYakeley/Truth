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
    | RunOption Bool
                (Maybe FilePath)
                [FilePath]

optParser :: O.Parser Options
optParser =
    (O.flag' ExprDocOption $ O.long "doc") <|>
    RunOption <$> (O.switch $ O.long "interactive" <> O.short 'i') <*> (O.optional $ O.strOption (O.long "data")) <*>
    (O.many $ O.strArgument mempty)

main :: IO ()
main =
    truthMain $ \args createWindow -> do
        options <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        case options of
            ExprDocOption -> do
                for_ (predefinedDoc @PinaforeEdit) $ \(name, desc) -> putStrLn $ (show name) ++ " :: " ++ unpack desc
                putStrLn $ "<file> :: " ++ unpack filePinaforeType
            RunOption fInteract mdirpath fpaths -> do
                dirpath <-
                    case mdirpath of
                        Just dirpath -> return dirpath
                        Nothing -> getUserDataDir "pinafore"
                createDirectoryIfMissing True dirpath
                let
                    doFile :: FilePath -> Text -> IO ()
                    doFile fpath text = sqlitePinaforeMain dirpath (fpath, text) createWindow
                case fpaths of
                    [] -> do
                        isterm <- hIsTerminalDevice stdin
                        if isterm || fInteract
                            then sqlitePinaforeInteractive dirpath createWindow
                            else do
                                ptext <- getContents
                                doFile "<stdin>" $ decodeUtf8 $ toStrict ptext
                    _ -> do
                        for_ fpaths $ \fpath -> do
                            ptext <- readFile fpath
                            doFile fpath $ decodeUtf8 $ toStrict ptext
                        if fInteract
                            then sqlitePinaforeInteractive dirpath createWindow
                            else return ()
