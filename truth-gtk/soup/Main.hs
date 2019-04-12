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
    truthMainGTK async $ \MkTruthContext {..} ->
        liftIO $ do
            (dirpaths, double) <-
                O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) tcArguments
            for_ dirpaths $ \dirpath -> do
                let action = soupWindow async tcUIToolkit dirpath
                action
                if double
                    then action
                    else return ()
