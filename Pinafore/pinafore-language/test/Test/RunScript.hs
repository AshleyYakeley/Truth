module Test.RunScript
    ( module Shapes.Test
    , module Shapes.Test.Context
    , ScriptTestTree
    , tDecls
    , tDeclsRec
    , tFetchModule
    , tPrefix
    , tOpenDefaultStore
    , testOpenUHStore
    , tModule
    , tLibrary
    , runScriptTestTree
    , testExpression
    , ScriptExpectation(..)
    , testScriptExpectation
    , testExpectSuccess
    , testExpectThrow
    , testExpectReject
    , testExpectStop
    ) where

import Data.Shim
import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test
import Shapes.Test.Context

data ScriptContext = MkScriptContext
    { scDeclarations :: [String]
    , scFetchModule :: FetchModule ()
    , scPrefix :: Text
    }

type ScriptTestTree = ContextTestTree ScriptContext

tDecls :: [String] -> ScriptTestTree -> ScriptTestTree
tDecls defs = tContext $ \sc -> sc {scDeclarations = scDeclarations sc <> defs}

tDeclsRec :: [String] -> ScriptTestTree -> ScriptTestTree
tDeclsRec defs = tDecls $ pure $ "rec\n" ++ intercalate ";\n" defs ++ "\nend"

tFetchModule :: FetchModule () -> ScriptTestTree -> ScriptTestTree
tFetchModule fm = tContext $ \sc -> sc {scFetchModule = fm <> scFetchModule sc}

tPrefix :: Text -> ScriptTestTree -> ScriptTestTree
tPrefix t = tContext $ \sc -> sc {scPrefix = scPrefix sc <> t <> "\n"}

tOpenDefaultStore :: ScriptTestTree -> ScriptTestTree
tOpenDefaultStore = tPrefix "Env.openDefaultStore >>= fn store =>"

testOpenUHStore :: ScriptTestTree -> ScriptTestTree
testOpenUHStore =
    tPrefix "Env.openDefaultStore >>= fn dstore =>" .
    tPrefix "Undo.newUndoHandler >>= fn undoHandler =>" . tPrefix "Undo.handleStore undoHandler dstore >>= fn store =>"

tModule :: Text -> Text -> ScriptTestTree -> ScriptTestTree
tModule name script =
    tFetchModule $
    textFetchModule $ \n ->
        return $
        if pack (show n) == name
            then Just script
            else Nothing

tLibrary :: LibraryModule () -> ScriptTestTree -> ScriptTestTree
tLibrary libm = tFetchModule $ libraryFetchModule [libm]

runScriptTestTree :: ScriptTestTree -> TestTree
runScriptTestTree =
    runContextTestTree $ let
        scDeclarations = mempty
        scFetchModule = mempty
        scPrefix = mempty
        in MkScriptContext {..}

declsPrefix :: [String] -> Text
declsPrefix c = pack $ "let\n" ++ intercalate ";\n" c ++ "\nin\n"

testExpression ::
       forall a. HasQType 'Negative a
    => Text
    -> Text
    -> (Tester a -> Tester ())
    -> ScriptTestTree
testExpression name script call =
    MkContextTestTree $ \MkScriptContext {..} ->
        testTree (unpack name) $ let
            fullscript = scPrefix <> declsPrefix scDeclarations <> script
            in runTester defaultTester {tstFetchModule = scFetchModule} $ do
                   call $ testerLiftInterpreter $ parseValueUnify fullscript

testScript :: Text -> Text -> (Tester (Action ()) -> Tester ()) -> ScriptTestTree
testScript = testExpression @(Action ())

testScriptCatchStop :: Text -> Text -> (Tester (Action ()) -> Tester ()) -> ScriptTestTree
testScriptCatchStop name script = testScript name $ "onStop (" <> script <> ") (fail \"stopped\")"

data ScriptExpectation
    = ScriptExpectRejection (PinaforeError -> Bool)
    | ScriptExpectRuntimeException (IOException -> Bool)
    | ScriptExpectStop
    | ScriptExpectSuccess

instance Show ScriptExpectation where
    show (ScriptExpectRejection _) = "reject"
    show (ScriptExpectRuntimeException _) = "runtime exception"
    show ScriptExpectStop = "stop"
    show ScriptExpectSuccess = "success"

testerAssertThrowsException ::
       forall ex a. Exception ex
    => (ex -> Bool)
    -> Tester a
    -> Tester ()
testerAssertThrowsException checkEx ta = liftIOWithUnlift $ \unlift -> assertThrowsException checkEx $ unlift ta

testScriptExpectation :: Text -> ScriptExpectation -> Text -> ScriptTestTree
testScriptExpectation name (ScriptExpectRejection checkEx) script =
    testScript name script $ \interpret -> testerAssertThrowsException checkEx interpret
testScriptExpectation name (ScriptExpectRuntimeException checkEx) script =
    testScript name script $ \interpret -> do
        action <- interpret
        testerAssertThrowsException checkEx $ testerRunAction action
testScriptExpectation name ScriptExpectStop script =
    testScriptCatchStop name script $ \interpret -> do
        action <- interpret
        testerAssertThrowsException @IOException (\err -> show err == "user error (stopped)") $ testerRunAction action
testScriptExpectation name ScriptExpectSuccess script =
    testScriptCatchStop name script $ \interpret -> do
        action <- interpret
        testerRunAction action

testExpectSuccess :: Text -> ScriptTestTree
testExpectSuccess script = testScriptExpectation script ScriptExpectSuccess script

testExpectThrow :: Text -> ScriptTestTree
testExpectThrow script = testScriptExpectation ("THROW: " <> script) (ScriptExpectRuntimeException $ pure True) script

testExpectReject :: Text -> ScriptTestTree
testExpectReject script = testScriptExpectation ("REJECT: " <> script) (ScriptExpectRejection $ pure True) script

testExpectStop :: Text -> ScriptTestTree
testExpectStop script = testScriptExpectation ("STOP: " <> script) ScriptExpectStop script
