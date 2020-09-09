module Main
    ( main
    ) where

import Shapes
import Test.Tasty

{- no tests currently
import Test.Tasty.HUnit
import Changes.Core
import Changes.UI.GTK
-}
tests :: TestTree
tests = testGroup "changes-gtk" []

main :: IO ()
main = defaultMain tests
