module Main
    ( main
    ) where

import Options
import Options.Applicative
import Options.Applicative.Help hiding ((</>))
import Shapes
import Shapes.Test

parseResult :: Show a => ParserResult a -> Result String a
parseResult (Success a) = SuccessResult a
parseResult r = FailureResult $ show r

testOptions :: [String] -> Result String Options -> TestTree
testOptions args expected =
    testTree (intercalate " " args) $ let
        found = parseResult $ execParserPure defaultPrefs optParserInfo args
        in assertEqual "" expected found

testOptionParsing :: TestTree
testOptionParsing =
    testTree
        "option-parsing"
        [ testOptions ["-v"] $ SuccessResult ShowVersionOption
        , testOptions ["--version"] $ SuccessResult ShowVersionOption
        , testTree
              "script"
              [ testOptions ["scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", [], [])
              , testOptions ["scriptname", "a"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", ["a"], [])
              , testOptions ["scriptname", "-x"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", ["-x"], [])
              , testOptions ["scriptname", "--opt"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", ["--opt"], [])
              , testOptions ["scriptname", "-n"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", ["-n"], [])
              , testOptions ["scriptname", "-v"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", ["-v"], [])
              , testOptions ["scriptname", "--data", "dpath"] $
                SuccessResult $
                RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", ["--data", "dpath"], [])
              , testOptions ["-n", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) True ("scriptname", [], [])
              , testOptions ["--sloppy", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing True) False ("scriptname", [], [])
              , testOptions ["--imply", "pqr=vv", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) False ("scriptname", [], [("pqr", "vv")])
              , testOptions ["--imply", "a=1", "--imply", "b=2", "--imply", "c=3", "scriptname"] $
                SuccessResult $
                RunFileOption
                    (MkRunOptions [] Nothing False)
                    False
                    ("scriptname", [], [("a", "1"), ("b", "2"), ("c", "3")])
              , testOptions ["-n", "scriptname", "-n"] $
                SuccessResult $ RunFileOption (MkRunOptions [] Nothing False) True ("scriptname", ["-n"], [])
              , testOptions ["-I", "incpath", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions ["incpath"] Nothing False) False ("scriptname", [], [])
              , testOptions ["-I", "path1", "-I", "path2", "scriptname"] $
                SuccessResult $
                RunFileOption (MkRunOptions ["path1", "path2"] Nothing False) False ("scriptname", [], [])
              , testOptions ["--include", "incpath", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions ["incpath"] Nothing False) False ("scriptname", [], [])
              , testOptions ["--include", "path1", "--include", "path2", "scriptname"] $
                SuccessResult $
                RunFileOption (MkRunOptions ["path1", "path2"] Nothing False) False ("scriptname", [], [])
              , testOptions ["--data", "dpath", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions [] (Just "dpath") False) False ("scriptname", [], [])
              , testOptions ["--data", "dpath", "scriptname", "arg1"] $
                SuccessResult $ RunFileOption (MkRunOptions [] (Just "dpath") False) False ("scriptname", ["arg1"], [])
              , testOptions ["--data", "dpath", "scriptname", "arg1", "arg2"] $
                SuccessResult $
                RunFileOption (MkRunOptions [] (Just "dpath") False) False ("scriptname", ["arg1", "arg2"], [])
              , testOptions ["-n", "--data", "dpath", "scriptname", "arg1", "arg2"] $
                SuccessResult $
                RunFileOption (MkRunOptions [] (Just "dpath") False) True ("scriptname", ["arg1", "arg2"], [])
              , testOptions ["-n", "--sloppy", "--data", "dpath", "scriptname", "arg1", "arg2"] $
                SuccessResult $
                RunFileOption (MkRunOptions [] (Just "dpath") True) True ("scriptname", ["arg1", "arg2"], [])
              ]
        , testTree
              "interactive"
              [ testOptions ["-i"] $ SuccessResult $ RunInteractiveOption $ MkRunOptions [] Nothing False
              , testOptions ["-i", "--data", "dpath"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions [] (Just "dpath") False
              , testOptions ["--interactive"] $ SuccessResult $ RunInteractiveOption $ MkRunOptions [] Nothing False
              , testOptions ["-I", "incpath", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions ["incpath"] Nothing False
              , testOptions ["--include", "incpath", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions ["incpath"] Nothing False
              , testOptions ["--interactive", "-I", "incpath"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions ["incpath"] Nothing False
              , testOptions ["-I", "path1", "-I", "path2", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions ["path1", "path2"] Nothing False
              , testOptions ["--include", "path1", "--include", "path2", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions ["path1", "path2"] Nothing False
              , testOptions ["--interactive", "-I", "path1", "-I", "path2"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions ["path1", "path2"] Nothing False
              , testOptions ["--interactive", "--data", "dpath"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions [] (Just "dpath") False
              , testOptions ["--interactive", "--sloppy", "--data", "dpath"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions [] (Just "dpath") True
              ]
        ]

testOptionHelp :: TestTree
testOptionHelp =
    testHandleVsFileInDir ("app/main/test") "option-help" $ \h -> do
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
