module Test.Scripts
    ( testScripts
    ) where

import Paths_pinafore_stdlib
import Pinafore
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
    runTester defaultTester {tstFetchModule = libraryFetchModule extraLibrary <> directoryFetchModule libDir} $ do
        _ <- testerLiftView $ qInterpretFile fpath
        return ()

testScripts :: TestTree
testScripts =
    testTree
        "scripts"
        [ testTree "test" $
          fmap
              (\name -> testCheckScript ("test/script" </> name) name)
              ["empty", "simple", "testquery", "test", "big-ui", "calendar", "drawing", "choose"]
        , testTree "example" $
          fmap
              (\name -> testCheckScript ("examples" </> name) name)
              ["stopwatch", "calendar", "contacts", "events", "clock", "fake-theme-system-journal"]
        ]
