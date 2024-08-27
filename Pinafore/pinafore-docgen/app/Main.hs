module Main
    ( main
    ) where

import Options
import Pinafore.DocGen
import Pinafore.Documentation
import Pinafore.Options
import Pinafore.Version
import Shapes

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        ModuleDocOption ropts modname -> do
            modopts <- getModuleOptions ropts
            generateCommonMarkDoc stdout modopts $ MkModuleName modname
