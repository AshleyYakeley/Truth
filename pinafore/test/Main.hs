{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Shapes
import Test.Language
import Test.Tasty

tests :: TestTree
tests = testGroup "pinafore" [testLanguage]

main :: IO ()
main = defaultMain tests
