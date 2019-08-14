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

ut :: UpdateTiming
ut = SynchronousUpdateTiming

main :: IO ()
main = do
    (dirpaths, double) <- O.execParser (O.info optParser mempty)
    truthMainGTK $ \MkTruthContext {..} -> do
        (toolkit, checkdone) <- liftIO $ quitOnWindowsClosed tcUIToolkit
        for_ dirpaths $ \dirpath -> do
            let action = soupWindow ut toolkit dirpath
            action
            if double
                then action
                else return ()
        liftIO checkdone
