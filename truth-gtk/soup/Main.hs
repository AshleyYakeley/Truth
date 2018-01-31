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
    truthMain $ \args -> do
        (dirpaths, double) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        wmss <-
            for dirpaths $ \dirpath -> do
                wm <- fmap MkSomeUIWindow $ soupWindow dirpath
                return $
                    if double
                        then [wm, wm]
                        else [wm]
        return $ mconcat wmss