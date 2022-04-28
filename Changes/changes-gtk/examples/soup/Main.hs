module Main
    ( main
    ) where

import Changes.Core
import Changes.UI.GTK
import qualified Options.Applicative as O
import Shapes
import Soup

optParser :: O.Parser ([FilePath], Bool)
optParser = (,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2')

main :: IO ()
main = do
    (dirpaths, double) <- O.execParser (O.info optParser mempty)
    changesMainGTK $ \tc -> do
        let newWindow spec = ccExitOnClosed tc $ createWindow spec
        for_ dirpaths $ \dirpath -> do
            let action = soupWindow newWindow dirpath
            action
            if double
                then action
                else return ()
