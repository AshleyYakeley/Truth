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
    truthMain $ \args createWindow ->
        liftIO $ do
            (dirpaths, double) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
            for_ dirpaths $ \dirpath -> do
                let action = soupWindow (\uiw -> createWindow uiw >> return ()) dirpath
                action
                if double
                    then action
                    else return ()
