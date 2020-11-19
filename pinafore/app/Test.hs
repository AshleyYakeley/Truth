module Main
    ( main
    ) where

import Options
import Options.Applicative
import Options.Applicative.Help hiding ((</>))
import Shapes
import Shapes.Test

testOptions :: [String] -> Maybe Options -> TestTree
testOptions args expected =
    testTree (intercalate " " args) $ let
        found = getParseResult $ execParserPure defaultPrefs optParserInfo args
        in assertEqual "" expected found

testOptionParsing :: TestTree
testOptionParsing =
    testTree
        "option-parsing"
        [ testOptions ["-v"] $ Just ShowVersionOption
        , testOptions ["--version"] $ Just ShowVersionOption
        , testOptions ["--doc-predefined"] $ Just PredefinedDocOption
        , testOptions ["--doc-infix"] $ Just InfixDocOption
        , testOptions ["--dump-table"] $ Just $ DumpTableOption Nothing
        , testOptions ["--dump-table", "--data", "dpath"] $ Just $ DumpTableOption $ Just "dpath"
        , testOptions ["-i"] $ Just $ RunInteractiveOption [] Nothing
        , testOptions ["-i", "--data", "dpath"] $ Just $ RunInteractiveOption [] $ Just "dpath"
        , testOptions ["--interactive"] $ Just $ RunInteractiveOption [] Nothing
        , testOptions ["-I", "incpath", "--interactive"] $ Just $ RunInteractiveOption ["incpath"] Nothing
        , testOptions ["--include", "incpath", "--interactive"] $ Just $ RunInteractiveOption ["incpath"] Nothing
        , testOptions ["--interactive", "-I", "incpath"] $ Just $ RunInteractiveOption ["incpath"] Nothing
        , testOptions ["-I", "path1", "-I", "path2", "--interactive"] $
          Just $ RunInteractiveOption ["path1", "path2"] Nothing
        , testOptions ["--include", "path1", "--include", "path2", "--interactive"] $
          Just $ RunInteractiveOption ["path1", "path2"] Nothing
        , testOptions ["--interactive", "-I", "path1", "-I", "path2"] $
          Just $ RunInteractiveOption ["path1", "path2"] Nothing
        , testOptions ["--interactive", "--data", "dpath"] $ Just $ RunInteractiveOption [] $ Just "dpath"
        , testOptions ["scriptname"] $ Just $ RunFileOption False [] Nothing $ Just ("scriptname", [])
        , testOptions ["-I", "incpath", "scriptname"] $
          Just $ RunFileOption False ["incpath"] Nothing $ Just ("scriptname", [])
        , testOptions ["-I", "path1", "-I", "path2", "scriptname"] $
          Just $ RunFileOption False ["path1", "path2"] Nothing $ Just ("scriptname", [])
        , testOptions ["--include", "incpath", "scriptname"] $
          Just $ RunFileOption False ["incpath"] Nothing $ Just ("scriptname", [])
        , testOptions ["--include", "path1", "--include", "path2", "scriptname"] $
          Just $ RunFileOption False ["path1", "path2"] Nothing $ Just ("scriptname", [])
        , testOptions ["--data", "dpath", "scriptname"] $
          Just $ RunFileOption False [] (Just "dpath") $ Just ("scriptname", [])
        , testOptions ["--data", "dpath", "scriptname", "arg1"] $
          Just $ RunFileOption False [] (Just "dpath") $ Just ("scriptname", ["arg1"])
        , testOptions ["--data", "dpath", "scriptname", "arg1", "arg2"] $
          Just $ RunFileOption False [] (Just "dpath") $ Just ("scriptname", ["arg1", "arg2"])
        , testOptions ["scriptname", "a"] $ Just $ RunFileOption False [] Nothing $ Just ("scriptname", ["a"])
        , testOptions ["scriptname", "-x"] $ Just $ RunFileOption False [] Nothing $ Just ("scriptname", ["-x"])
        , testOptions ["scriptname", "--opt"] $ Just $ RunFileOption False [] Nothing $ Just ("scriptname", ["--opt"])
        , testOptions ["scriptname", "-n"] $ Just $ RunFileOption False [] Nothing $ Just ("scriptname", ["-n"])
        , testOptions ["scriptname", "-v"] $ Just $ RunFileOption False [] Nothing $ Just ("scriptname", ["-v"])
        , testOptions ["-n", "scriptname"] $ Just $ RunFileOption True [] Nothing $ Just ("scriptname", [])
        , testOptions ["-n", "scriptname", "-n"] $ Just $ RunFileOption True [] Nothing $ Just ("scriptname", ["-n"])
        ]

testOptionHelp :: TestTree
testOptionHelp =
    testHandleVsFile ("test" </> "golden") "option-help" $ \h -> do
        let pr = execParserPure defaultPrefs optParserInfo ["--help"]
        case pr of
            Failure (ParserFailure f) ->
                case f "pinafore" of
                    (ph, _, _) -> hPutStrLn h $ renderHelp 80 ph
            _ -> assertFailure "parser didn't fail"

tests :: TestTree
tests = testTree "app" [testOptionParsing, testOptionHelp]

main :: IO ()
main = testMain tests
