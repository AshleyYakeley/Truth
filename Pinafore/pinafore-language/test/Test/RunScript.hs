module Test.RunScript
    ( module Shapes.Test
    , module Shapes.Test.Context
    , ScriptTestTree
    , tDecls
    , tDeclsRec
    , tFetchModule
    , tPrefix
    , tDeclarator
    , tWith
    , tImport
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
    { scFetchModule :: FetchModule ()
    , scPrefix :: Text
    }

type ScriptTestTree = ContextTestTree ScriptContext

tFetchModule :: FetchModule () -> ScriptTestTree -> ScriptTestTree
tFetchModule fm = tContext $ \sc -> sc {scFetchModule = fm <> scFetchModule sc}

tPrefix :: Text -> ScriptTestTree -> ScriptTestTree
tPrefix t = tContext $ \sc -> sc {scPrefix = scPrefix sc <> t <> "\n"}

tDecls :: [String] -> ScriptTestTree -> ScriptTestTree
tDecls defs = tPrefix $ pack $ "let\n" <> intercalate ";\n" defs <> "\nin\n"

tDeclsRec :: [String] -> ScriptTestTree -> ScriptTestTree
tDeclsRec defs = tPrefix $ pack $ "let rec\n" <> intercalate ";\n" defs <> "\nin\n"

tDeclarator :: Text -> ScriptTestTree -> ScriptTestTree
tDeclarator t = tPrefix $ t <> " in\n"

tWith :: [Text] -> ScriptTestTree -> ScriptTestTree
tWith tt = tDeclarator $ "with " <> intercalate ", " tt

tImport :: [Text] -> ScriptTestTree -> ScriptTestTree
tImport tt = tDeclarator $ "import " <> intercalate ", " (fmap (pack . show) tt)

tOpenDefaultStore :: ScriptTestTree -> ScriptTestTree
tOpenDefaultStore = tPrefix "openDefault.Store >>=.Action fn store =>"

testOpenUHStore :: ScriptTestTree -> ScriptTestTree
testOpenUHStore =
    tPrefix "openDefault.Store >>=.Action fn dstore =>" .
    tPrefix "new.UndoHandler >>=.Action fn undoHandler =>" .
    tPrefix "handleStore.UndoHandler undoHandler dstore >>=.Action fn store =>"

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
        scFetchModule = mempty
        scPrefix = mempty
        in MkScriptContext {..}

testExpression ::
       forall a. HasQType 'Negative a
    => Text
    -> Text
    -> (Tester a -> Tester ())
    -> ScriptTestTree
testExpression name script call =
    MkContextTestTree $ \MkScriptContext {..} ->
        testTree (unpack name) $ let
            fullscript = scPrefix <> script
            in runTester defaultTester {tstFetchModule = scFetchModule} $ do
                   call $ testerLiftInterpreter $ parseValueUnify fullscript

testScript :: Text -> Text -> (Tester (Action ()) -> Tester ()) -> ScriptTestTree
testScript = testExpression @(Action ())

testScriptCatchStop :: Text -> Text -> (Tester (Action ()) -> Tester ()) -> ScriptTestTree
testScriptCatchStop name script = testScript name $ "onStop.Action. (" <> script <> ") (fail.Action. \"stopped\")"

data ScriptExpectation
    = ScriptExpectRejection (QError -> Bool)
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
