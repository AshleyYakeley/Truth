module Test.RunScript
    ( module Shapes.Test
    , module Shapes.Test.Context
    , ScriptTestTree
    , tDecls
    , tFetchModule
    , tModule
    , tLibrary
    , runScriptTestTree
    , ScriptExpectation(..)
    , testPinaforeScript
    , testExpectSuccess
    , testExpectThrow
    , testExpectReject
    , testExpectStop
    ) where

import Changes.Core
import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test
import Shapes.Test.Context

data ScriptContext = MkScriptContext
    { scDeclarations :: [String]
    , scFetchModule :: FetchModule
    }

type ScriptTestTree = ContextTestTree ScriptContext

tDecls :: [String] -> ScriptTestTree -> ScriptTestTree
tDecls defs = tContext $ \sc -> sc {scDeclarations = scDeclarations sc <> defs}

tFetchModule :: FetchModule -> ScriptTestTree -> ScriptTestTree
tFetchModule fm = tContext $ \sc -> sc {scFetchModule = fm <> scFetchModule sc}

tModule :: Text -> Text -> ScriptTestTree -> ScriptTestTree
tModule name script =
    tFetchModule $
    textFetchModule $ \n ->
        return $
        if pack (show n) == name
            then Just script
            else Nothing

tLibrary :: LibraryModule -> ScriptTestTree -> ScriptTestTree
tLibrary libm = tFetchModule $ libraryFetchModule [libm]

runScriptTestTree :: ScriptTestTree -> TestTree
runScriptTestTree =
    runContextTestTree $ let
        scDeclarations = mempty
        scFetchModule = mempty
        in MkScriptContext {..}

prefix :: [String] -> Text
prefix c = pack $ "let\n" ++ intercalate ";\n" c ++ "\nin\n"

data ScriptExpectation
    = ScriptExpectRejection (PinaforeError -> Bool)
    | ScriptExpectRuntimeException (IOException -> Bool)
    | ScriptExpectStop
    | ScriptExpectSuccess
    | forall a. FromPinaforeType a =>
                    ScriptExpectSuccessResult ((?pinafore :: PinaforeContext) => ChangesContext -> a -> LifeCycle ())

testPinaforeScript :: Text -> ScriptExpectation -> Text -> ScriptTestTree
testPinaforeScript name expect script =
    MkContextTestTree $ \MkScriptContext {..} ->
        testTree (unpack name) $ let
            fullscript = prefix scDeclarations <> script
            runTest ::
                   forall a. (FromPinaforeType a, ?pinafore :: PinaforeContext, ?library :: LibraryContext)
                => IO (PinaforeAction a)
            runTest = throwInterpretResult $ pinaforeInterpretTextAtType "<test>" fullscript
            runTestCatchStop ::
                   forall a. (FromPinaforeType a, ?pinafore :: PinaforeContext, ?library :: LibraryContext)
                => IO (PinaforeAction a)
            runTestCatchStop =
                throwInterpretResult $
                pinaforeInterpretTextAtType "<test>" $ "onStop (" <> fullscript <> ") (fail \"stopped\")"
            in withTestPinaforeContext scFetchModule stdout $ \cc unlift _getTableState ->
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
                           r <-
                               ccUnliftLifeCycle cc $
                               ccRunView cc emptyResourceContext $ unliftPinaforeActionOrFail action
                           unlift $ checkResult cc r

testExpectSuccess :: Text -> ScriptTestTree
testExpectSuccess script = testPinaforeScript script ScriptExpectSuccess script

testExpectThrow :: Text -> ScriptTestTree
testExpectThrow script = testPinaforeScript ("THROW: " <> script) (ScriptExpectRuntimeException $ pure True) script

testExpectReject :: Text -> ScriptTestTree
testExpectReject script = testPinaforeScript ("REJECT: " <> script) (ScriptExpectRejection $ pure True) script

testExpectStop :: Text -> ScriptTestTree
testExpectStop script = testPinaforeScript ("STOP: " <> script) ScriptExpectStop script
