module Main
    ( main
    ) where

import Shapes
import Test.Tasty

{- no tests currently
import Test.Tasty.HUnit
import Truth.Core
import Truth.UI.GTK
-}
tests :: TestTree
tests = testGroup "truth-gtk" []

main :: IO ()
main = defaultMain tests
