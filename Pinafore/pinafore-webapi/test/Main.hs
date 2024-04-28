module Main
    ( main
    ) where

import Pinafore
import Pinafore.Test
import Pinafore.WebAPI
import Shapes
import Shapes.Test

testSchema :: TestTree
testSchema =
    testTree "schema" $
    runTester defaultTester {tstImporters = webAPIImporters} $ do
        sctext :: [Text] <-
            testerLiftInterpreter $
            -- https://github.com/OAI/OpenAPI-Specification/blob/main/examples/v3.0/petstore-expanded.json
            parseValueUnify "import openapi \"file:test/schema/petstore-expanded.json\" in servers.Info."
        liftIO $ assertEqual "" [] sctext

tests :: [TestTree]
tests = [testSchema]

main :: IO ()
main = testMainNoSignalHandler $ testTree "pinafore-webapi" tests
