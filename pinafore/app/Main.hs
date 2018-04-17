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
        options <- liftIO $ O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        case options of
            ExprDocOption ->
                liftIO $ do
                    for_ (predefinedDoc @PinaforeEdit) $ \(name, desc) ->
                        putStrLn $ (show name) ++ " :: " ++ unpack desc
                    putStrLn $ "<file> :: " ++ unpack filePinaforeType
            RunOption fInteract mdirpath fpaths -> do
                dirpath <-
                    case mdirpath of
                        Just dirpath -> return dirpath
                        Nothing -> liftIO $ getUserDataDir "pinafore"
                liftIO $ createDirectoryIfMissing True dirpath
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
