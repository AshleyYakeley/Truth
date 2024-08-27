module Test.Scripts
    ( testScripts
    ) where

import Paths_pinafore_lib_script
import Pinafore.Libs
import Pinafore.Test
import Shapes
import Shapes.Test
import Shapes.Unsafe (unsafePerformIO)

libDir :: FilePath
libDir = unsafePerformIO getDataDir

-- Just check, don't run
testCheckScript :: FilePath -> String -> TestTree
testCheckScript fpath name =
    testTree name $
    runTester defaultTester $
    testerLoadLibrary appLibrary $
    testerLoad (directoryLoadModule libDir) $ do
        _ <- testerInterpretScriptFile fpath []
        return ()

testQuine :: TestName -> FilePath -> FilePath -> TestTree
testQuine name fpath outpath =
    testHandleVsFile name fpath outpath $ \hout ->
        runTester defaultTester {tstOutput = hout} $ do
            action <- testerInterpretScriptFile fpath []
            testerLiftView action

testScripts :: TestTree
testScripts =
    testTree
        "scripts"
        [ testTree "test" $
          fmap
              (\name -> testCheckScript ("test/script" </> name) name)
              ["empty", "simple", "testquery", "test", "big-ui", "calendar", "drawing", "choose", "notes"]
        , testTree "example" $
          fmap
              (\name -> testCheckScript ("examples" </> name) name)
              [ "stopwatch"
              , "calendar"
              , "contacts"
              , "events"
              , "clock"
              , "quine"
              , "fake-theme-system-journal"
              , "webkit"
              , "commonmark"
              ]
        , testQuine "quine" "examples/quine" "test/out/quine.out"
        ]
