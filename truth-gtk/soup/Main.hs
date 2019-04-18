module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Shapes
import Soup
import Truth.Core
import Truth.UI.GTK

optParser :: O.Parser ([FilePath], Bool)
optParser = (,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2')

async :: Bool
async = False

main :: IO ()
main =
    truthMainGTK $ \MkTruthContext {..} -> do
        (dirpaths, double) <-
            liftIO $ O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) tcArguments
        toolkit <- liftIO $ quitOnWindowsClosed tcUIToolkit
        for_ dirpaths $ \dirpath -> do
            let action = soupWindow async toolkit dirpath
            action
            if double
                then action
                else return ()
