module Main
    ( main
    ) where

import Options
import Options.Applicative
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

testOptions :: [String] -> Maybe Options -> TestTree
testOptions args expected =
    testCase (intercalate " " args) $ let
        found = getParseResult $ execParserPure (prefs mempty) optParserInfo args
        in assertEqual "" expected found

testOptionParsing :: TestTree
testOptionParsing =
    testGroup
        "option-parsing"
        [ testOptions ["-v"] $ Just ShowVersionOption
        , testOptions ["--version"] $ Just ShowVersionOption
        , testOptions ["--doc-predefined"] $ Just PredefinedDocOption
        , testOptions ["--doc-infix"] $ Just InfixDocOption
        , testOptions ["--dump-table"] $ Just $ DumpTableOption Nothing
        , testOptions ["--dump-table", "--data", "dpath"] $ Just $ DumpTableOption $ Just "dpath"
        , testOptions ["-i"] $ Just $ RunInteractiveOption Nothing
        , testOptions ["-i", "--data", "dpath"] $ Just $ RunInteractiveOption $ Just "dpath"
        , testOptions ["--interactive"] $ Just $ RunInteractiveOption Nothing
        , testOptions ["--interactive", "--data", "dpath"] $ Just $ RunInteractiveOption $ Just "dpath"
        , testOptions ["scriptname"] $ Just $ RunFileOption False Nothing $ Just ("scriptname", [])
        , testOptions ["scriptname", "a"] $ Just $ RunFileOption False Nothing $ Just ("scriptname", ["a"])
        , testOptions ["scriptname", "-x"] $ Just $ RunFileOption False Nothing $ Just ("scriptname", ["-x"])
        , testOptions ["scriptname", "--opt"] $ Just $ RunFileOption False Nothing $ Just ("scriptname", ["--opt"])
        , testOptions ["scriptname", "-n"] $ Just $ RunFileOption False Nothing $ Just ("scriptname", ["-n"])
        , testOptions ["scriptname", "-v"] $ Just $ RunFileOption False Nothing $ Just ("scriptname", ["-v"])
        , testOptions ["-n", "scriptname"] $ Just $ RunFileOption True Nothing $ Just ("scriptname", [])
        , testOptions ["-n", "scriptname", "-n"] $ Just $ RunFileOption True Nothing $ Just ("scriptname", ["-n"])
        ]

tests :: TestTree
tests = testGroup "app" [testOptionParsing]

main :: IO ()
main = defaultMain tests
