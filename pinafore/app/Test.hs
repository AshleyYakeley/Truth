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
        , testOptions ["--doc-predefined"] $ SuccessResult PredefinedDocOption
        , testOptions ["--doc-infix"] $ SuccessResult InfixDocOption
        , testOptions ["--dump-table"] $ SuccessResult $ DumpTableOption Nothing
        , testOptions ["--dump-table", "--data", "dpath"] $ SuccessResult $ DumpTableOption $ Just "dpath"
        , testTree
              "script"
              [ testOptions ["scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) False ("scriptname", [])
              , testOptions ["scriptname", "a"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) False ("scriptname", ["a"])
              , testOptions ["scriptname", "-x"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) False ("scriptname", ["-x"])
              , testOptions ["scriptname", "--opt"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) False ("scriptname", ["--opt"])
              , testOptions ["scriptname", "-n"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) False ("scriptname", ["-n"])
              , testOptions ["scriptname", "-v"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) False ("scriptname", ["-v"])
              , testOptions ["scriptname", "--data", "dpath"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) False ("scriptname", ["--data", "dpath"])
              , testOptions ["-n", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) True ("scriptname", [])
              , testOptions ["-n", "scriptname", "-n"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] Nothing) True ("scriptname", ["-n"])
              , testOptions ["-I", "incpath", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions True ["incpath"] Nothing) False ("scriptname", [])
              , testOptions ["-I", "path1", "-I", "path2", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions True ["path1", "path2"] Nothing) False ("scriptname", [])
              , testOptions ["--include", "incpath", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions True ["incpath"] Nothing) False ("scriptname", [])
              , testOptions ["--include", "path1", "--include", "path2", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions True ["path1", "path2"] Nothing) False ("scriptname", [])
              , testOptions ["--data", "dpath", "scriptname"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] (Just "dpath")) False ("scriptname", [])
              , testOptions ["--data", "dpath", "scriptname", "arg1"] $
                SuccessResult $ RunFileOption (MkRunOptions True [] (Just "dpath")) False ("scriptname", ["arg1"])
              , testOptions ["--data", "dpath", "scriptname", "arg1", "arg2"] $
                SuccessResult $
                RunFileOption (MkRunOptions True [] (Just "dpath")) False ("scriptname", ["arg1", "arg2"])
              , testOptions ["-n", "--data", "dpath", "scriptname", "arg1", "arg2"] $
                SuccessResult $
                RunFileOption (MkRunOptions True [] (Just "dpath")) True ("scriptname", ["arg1", "arg2"])
              ]
        , testTree
              "interactive"
              [ testOptions ["-i"] $ SuccessResult $ RunInteractiveOption $ MkRunOptions True [] Nothing
              , testOptions ["-i", "--data", "dpath"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True [] $ Just "dpath"
              , testOptions ["--interactive"] $ SuccessResult $ RunInteractiveOption $ MkRunOptions True [] Nothing
              , testOptions ["-I", "incpath", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True ["incpath"] Nothing
              , testOptions ["--include", "incpath", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True ["incpath"] Nothing
              , testOptions ["--interactive", "-I", "incpath"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True ["incpath"] Nothing
              , testOptions ["-I", "path1", "-I", "path2", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True ["path1", "path2"] Nothing
              , testOptions ["--include", "path1", "--include", "path2", "--interactive"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True ["path1", "path2"] Nothing
              , testOptions ["--interactive", "-I", "path1", "-I", "path2"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True ["path1", "path2"] Nothing
              , testOptions ["--interactive", "--data", "dpath"] $
                SuccessResult $ RunInteractiveOption $ MkRunOptions True [] $ Just "dpath"
              ]
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
