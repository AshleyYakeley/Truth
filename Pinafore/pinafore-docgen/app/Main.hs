module Main
    ( main
    ) where

import Options
import Pinafore.DocGen
import Pinafore.Options
import Pinafore.Version
import Shapes

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        ModuleDocOption ropts modname -> do
            (_, modopts) <- getStorageModelOptions ropts
            generateCommonMarkDoc stdout modopts modname
