module Main
    ( main
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import qualified Options.Applicative as O
import Shapes
import Soup

optParser :: O.Parser ([FilePath], Bool)
optParser = (,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2')

main :: IO ()
main = do
    (dirpaths, double) <- O.execParser (O.info optParser mempty)
    runLifecycle $
        runGTK $ \gtkContext ->
            runNewView $
            runGView gtkContext $ do
                gvRunLocked $ do
                    for_ dirpaths $ \dirpath -> do
                        let action = soupWindow createWindow dirpath
                        action
                        if double
                            then action
                            else return ()
