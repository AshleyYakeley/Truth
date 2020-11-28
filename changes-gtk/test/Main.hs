module Main
    ( main
    ) where

import Shapes
import Shapes.Test

{- no tests currently
import Changes.Core
import Changes.UI.GTK
-}
tests :: TestTree
tests = testTree "changes-gtk" ([] @TestTree)

main :: IO ()
main = testMain tests
