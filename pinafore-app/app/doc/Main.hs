module Main
    ( main
    ) where

import Documentation
import Options
import Pinafore.Libs
import Pinafore.Version
import Shapes

main :: IO ()
main =
    getOptions >>= \case
        ShowVersionOption -> printVersion
        LibraryDocOption dir -> printLibraryBindings extraLibrary dir
        InfixDocOption -> printInfixOperatorTable
