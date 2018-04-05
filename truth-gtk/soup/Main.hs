module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Shapes
import Soup
import Truth.UI.GTK

optParser :: O.Parser ([FilePath], Bool)
optParser = (,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2')

main :: IO ()
main =
    truthMain $ \args createWindow -> do
        (dirpaths, double) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        for_ dirpaths $ \dirpath -> do
            w <- soupWindow dirpath
            createWindow w
            if double
                then createWindow w
                else return ()
