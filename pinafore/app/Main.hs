module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Pinafore.Main
import Shapes
import Truth.UI.GTK

optParser :: O.Parser (FilePath, [FilePath])
optParser = (,) <$> (O.strOption $ O.long "db") <*> (O.many $ O.strArgument mempty)

main :: IO ()
main =
    truthMain $ \args createWindow -> do
        (dirpath, puipaths) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        for_ puipaths $ \puipath -> do
            puitext <- readFile puipath
            sqlitePinaforeMain dirpath (puipath, decodeUtf8 $ toStrict puitext) createWindow
