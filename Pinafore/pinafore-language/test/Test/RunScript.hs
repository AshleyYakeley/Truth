module Test.RunScript
    ( module Shapes.Test
    , module Shapes.Test.Context
    , ScriptTestTree
    , tDecls
    , tDeclsRec
    , tFetchModule
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

import Changes.Core
import Data.Shim
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

tDeclsRec :: [String] -> ScriptTestTree -> ScriptTestTree
tDeclsRec defs = tDecls $ pure $ "rec\n" ++ intercalate ";\n" defs ++ "\nend"

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

testExpression ::
       forall a. HasPinaforeType 'Negative a
    => Text
    -> Text
    -> ((?pinafore :: PinaforeContext) => ChangesContext -> IO a -> LifeCycle ())
    -> ScriptTestTree
testExpression name script call =
    MkContextTestTree $ \MkScriptContext {..} ->
        testTree (unpack name) $ let
            fullscript = prefix scDeclarations <> script
            in withTestPinaforeContext scFetchModule stdout $ \cc _getTableState ->
                   ccUnliftLifeCycle cc $
                   call cc $ throwInterpretResult $ pinaforeInterpretTextAtType "<test>" fullscript

testScript ::
       Text
    -> Text
    -> ((?pinafore :: PinaforeContext) => ChangesContext -> IO (PinaforeAction ()) -> IO ())
    -> ScriptTestTree
testScript name script call =
    testExpression @(PinaforeAction ()) name script $ \cc interpret -> liftIO $ call cc interpret

testScriptCatchStop ::
       Text
    -> Text
    -> ((?pinafore :: PinaforeContext) => ChangesContext -> IO (PinaforeAction ()) -> IO ())
    -> ScriptTestTree
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

testScriptExpectation :: Text -> ScriptExpectation -> Text -> ScriptTestTree
testScriptExpectation name (ScriptExpectRejection checkEx) script =
    testScript name script $ \_ interpret -> assertThrowsException checkEx interpret
testScriptExpectation name (ScriptExpectRuntimeException checkEx) script =
    testScript name script $ \cc interpret -> do
        action <- interpret
        assertThrowsException checkEx $ ccRunView cc emptyResourceContext $ runPinaforeAction action
testScriptExpectation name ScriptExpectStop script =
    testScriptCatchStop name script $ \cc interpret -> do
        action <- interpret
        assertThrowsException @IOException (\err -> show err == "user error (stopped)") $
            ccRunView cc emptyResourceContext $ runPinaforeAction action
testScriptExpectation name ScriptExpectSuccess script =
    testScriptCatchStop name script $ \cc interpret -> do
        action <- interpret
        ccRunView cc emptyResourceContext $ runPinaforeAction action

testExpectSuccess :: Text -> ScriptTestTree
testExpectSuccess script = testScriptExpectation script ScriptExpectSuccess script

testExpectThrow :: Text -> ScriptTestTree
testExpectThrow script = testScriptExpectation ("THROW: " <> script) (ScriptExpectRuntimeException $ pure True) script

testExpectReject :: Text -> ScriptTestTree
testExpectReject script = testScriptExpectation ("REJECT: " <> script) (ScriptExpectRejection $ pure True) script

testExpectStop :: Text -> ScriptTestTree
testExpectStop script = testScriptExpectation ("STOP: " <> script) ScriptExpectStop script
