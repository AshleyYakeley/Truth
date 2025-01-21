module Main
    ( main
    )
where

import Pinafore.Documentation
import Pinafore.Options
import Pinafore.Version
import Shapes

import Options
import Pinafore.DocGen

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        ModuleDocOption ropts modname -> do
            modopts <- getModuleOptions ropts
            generateCommonMarkDoc stdout modopts $ MkModuleName modname
