module Main
    ( main
    )
where

import Changes.Core
import Options.Applicative qualified as O
import Shapes

import Changes.World.GNOME.GTK
import Soup

optParser :: O.Parser ([FilePath], Bool)
optParser = (,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2')

main :: IO ()
main = do
    (dirpaths, double) <- O.execParser (O.info optParser mempty)
    runLifecycle
        $ runGTK
        $ \gtkContext ->
            runView
                $ runGView gtkContext
                $ for_ dirpaths
                $ \dirpath -> do
                    let action = soupWindow createWindow dirpath
                    action
                    if double
                        then action
                        else return ()
