module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Pinafore
import Shapes
import Truth.UI.GTK

data Options
    = ExprDocOption
    | RunOption FilePath
                [FilePath]

optParser :: O.Parser Options
optParser =
    (O.flag' ExprDocOption $ O.long "doc") <|>
    RunOption <$> (O.strOption (O.long "db")) <*> (O.many $ O.strArgument mempty)

main :: IO ()
main =
    truthMain $ \args createWindow -> do
        options <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        case options of
            ExprDocOption -> do
                for_ (predefinedDoc @PinaforeEdit) $ \(name, desc) -> putStrLn $ (show name) ++ " :: " ++ unpack desc
                putStrLn $ "<file> :: " ++ unpack filePinaforeType
            RunOption dirpath fpaths -> let
                doFile :: FilePath -> Text -> IO ()
                doFile fpath text = sqlitePinaforeMain dirpath (fpath, text) createWindow
                in case fpaths of
                       [] -> do
                           ptext <- getContents
                           doFile "<stdin>" $ decodeUtf8 $ toStrict ptext
                       _ ->
                           for_ fpaths $ \fpath -> do
                               ptext <- readFile fpath
                               doFile fpath $ decodeUtf8 $ toStrict ptext
