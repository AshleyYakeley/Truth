module Test.RunScript where

import Changes.Core
import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test
import Test.Context

data ScriptExpectation
    = ScriptExpectRejection (PinaforeError -> Bool)
    | ScriptExpectRuntimeException (IOException -> Bool)
    | ScriptExpectStop
    | ScriptExpectSuccess
    | forall a. FromPinaforeType a =>
                    ScriptExpectSuccessResult ((?pinafore :: PinaforeContext) => ChangesContext -> a -> LifeCycle ())

testPinaforeScript :: Text -> FetchModule -> Text -> ScriptExpectation -> ContextTestTree
testPinaforeScript name fetchModule text expect =
    contextTestCase name text $ \t -> let
        runTest ::
               forall a. (FromPinaforeType a, ?pinafore :: PinaforeContext, ?library :: LibraryContext)
            => IO (PinaforeAction a)
        runTest = throwInterpretResult $ pinaforeInterpretTextAtType "<test>" t
        runTestCatchStop ::
               forall a. (FromPinaforeType a, ?pinafore :: PinaforeContext, ?library :: LibraryContext)
            => IO (PinaforeAction a)
        runTestCatchStop =
            throwInterpretResult $ pinaforeInterpretTextAtType "<test>" $ "onStop (" <> t <> ") (fail \"stopped\")"
        in withTestPinaforeContext fetchModule stdout $ \cc unlift _getTableState ->
               case expect of
                   ScriptExpectRejection checkEx -> assertThrowsException checkEx $ runTest @()
                   ScriptExpectRuntimeException checkEx -> do
                       action <- runTest @()
                       assertThrowsException checkEx $ ccRunView cc emptyResourceContext $ runPinaforeAction action
                   ScriptExpectStop -> do
                       action <- runTestCatchStop @()
                       assertThrowsException @IOException (\err -> show err == "user error (stopped)") $
                           ccRunView cc emptyResourceContext $ runPinaforeAction action
                   ScriptExpectSuccess -> do
                       action <- runTestCatchStop @()
                       ccRunView cc emptyResourceContext $ runPinaforeAction action
                   ScriptExpectSuccessResult (checkResult :: ChangesContext -> a -> LifeCycle ()) -> do
                       action <- runTestCatchStop @a
                       r <- ccUnliftLifeCycle cc $ ccRunView cc emptyResourceContext $ unliftPinaforeActionOrFail action
                       unlift $ checkResult cc r

testExpectSuccess :: Text -> ContextTestTree
testExpectSuccess text = testPinaforeScript text mempty text ScriptExpectSuccess

testExpectThrow :: Text -> ContextTestTree
testExpectThrow text = testPinaforeScript text mempty text $ ScriptExpectRuntimeException $ pure True

testExpectReject :: Text -> ContextTestTree
testExpectReject text = testPinaforeScript text mempty text $ ScriptExpectRejection $ pure True

testExpectStop :: Text -> ContextTestTree
testExpectStop text = testPinaforeScript text mempty text ScriptExpectStop
