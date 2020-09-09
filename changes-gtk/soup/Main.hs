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

main :: IO ()
main = do
    (dirpaths, double) <- O.execParser (O.info optParser mempty)
    truthMainGTK $ \tc -> do
        let newWindow spec = tcExitOnClosed tc $ createWindow spec
        for_ dirpaths $ \dirpath -> do
            let action = soupWindow tc newWindow dirpath
            action
            if double
                then action
                else return ()
