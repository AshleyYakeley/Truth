module Test.RunScript
    ( module Shapes.Test
    , module Shapes.Test.Context
    , ScriptTestTree
    , tDecls
    , tDeclsRec
    , tLoadModule
    , tPrefix
    , tDeclarator
    , tWith
    , tImport
    , tOpenDefaultStore
    , testOpenUHStore
    , tModule
    , tLibrary
    , tParallel
    , scriptRepeat
    , scriptAsync
    , runScriptTestTree
    , testExpression
    , ScriptExpectation (..)
    , testScriptExpectation
    , testExpectSuccess
    , testExpectThrow
    , testExpectReject
    , testExpectStop
    )
where

import Data.Shim
import Shapes
import Shapes.Test
import Shapes.Test.Context

import Pinafore.Main
import Pinafore.Test.Internal

data ScriptContext = MkScriptContext
    { scTesterOptions :: TesterOptions
    , scLoadModule :: LoadModule
    , scPrefix :: Text
    }

type ScriptTestTree = ContextTestTree ScriptContext

tLoadModule :: LoadModule -> ScriptTestTree -> ScriptTestTree
tLoadModule fm = tContext $ \sc -> sc{scLoadModule = fm <> scLoadModule sc}

tPrefix :: Text -> ScriptTestTree -> ScriptTestTree
tPrefix t = tContext $ \sc -> sc{scPrefix = scPrefix sc <> t <> "\n"}

tDecls :: [String] -> ScriptTestTree -> ScriptTestTree
tDecls defs = tPrefix $ pack $ "let {\n" <> intercalate ";\n" defs <> "\n}\n"

tDeclsRec :: [String] -> ScriptTestTree -> ScriptTestTree
tDeclsRec defs = tPrefix $ pack $ "let rec {\n" <> intercalate ";\n" defs <> "\n}\n"

tDeclarator :: Text -> ScriptTestTree -> ScriptTestTree
tDeclarator t = tPrefix $ t <> "\n"

tWith :: [Text] -> ScriptTestTree -> ScriptTestTree
tWith tt = tDeclarator $ "with " <> intercalate ", " tt

tImport :: [Text] -> ScriptTestTree -> ScriptTestTree
tImport tt = tDeclarator $ "import " <> intercalate ", " (fmap (pack . show) tt)

tOpenDefaultStore :: ScriptTestTree -> ScriptTestTree
tOpenDefaultStore = tPrefix "?openTestStore >>=.Action fn store =>"

testOpenUHStore :: ScriptTestTree -> ScriptTestTree
testOpenUHStore =
    tPrefix "?openTestStore >>=.Action fn dstore =>"
        . tPrefix "new.UndoHandler >>=.Action fn undoHandler =>"
        . tPrefix "handleStore.UndoHandler undoHandler dstore >>=.Action fn store =>"

tModule :: Text -> Text -> ScriptTestTree -> ScriptTestTree
tModule name script =
    tLoadModule
        $ textLoadModule
        $ \n ->
            return
                $ if pack (show n) == name
                    then Just script
                    else Nothing

tParallel :: ScriptTestTree -> ScriptTestTree
tParallel =
    tContext $ \sc -> let
        topts = scTesterOptions sc
        eopts = tstExecutionOptions topts
        in sc{scTesterOptions = topts{tstExecutionOptions = eopts{eoProcessorCount = Just AllProcessorCount}}}

tLibrary :: LibraryModule -> ScriptTestTree -> ScriptTestTree
tLibrary libm = tLoadModule $ libraryLoadModule [libm]

scriptRepeat :: Int -> Text -> Text
scriptRepeat i script = "for_ (range 1 " <> showText i <> ") $ fn _ => " <> script

scriptAsync :: Int -> Text -> Text
scriptAsync i script = scriptRepeat i $ "map.Action (fn _ => ()) $ async.Task. $ " <> script

runScriptTestTree :: ScriptTestTree -> TestTree
runScriptTestTree =
    runContextTestTree $ let
        scTesterOptions = defaultTester
        scLoadModule = mempty
        scPrefix = mempty
        in MkScriptContext{..}

testExpression ::
    forall a.
    HasQType QPolyShim 'Negative a =>
    Text ->
    Text ->
    (Tester a -> Tester ()) ->
    ScriptTestTree
testExpression name script call =
    MkContextTestTree $ \MkScriptContext{..} ->
        testTree (unpack name) $ let
            fullscript = scPrefix <> script
            in runTester scTesterOptions $ testerLoad scLoadModule $ call $ testerInterpret fullscript

testScript :: Text -> Text -> (Tester (Action ()) -> Tester ()) -> ScriptTestTree
testScript = testExpression @(Action ())

testScriptCatchStop :: Text -> Text -> (Tester (Action ()) -> Tester ()) -> ScriptTestTree
testScriptCatchStop name script = testScript name $ "onStop.Action. (" <> script <> ") (fail.Action. \"stopped\")"

data ScriptExpectation
    = ScriptExpectRejection (QLocatedError -> Bool)
    | ScriptExpectRuntimeException (IOException -> Bool)
    | ScriptExpectStop
    | ScriptExpectSuccess

instance Show ScriptExpectation where
    show (ScriptExpectRejection _) = "reject"
    show (ScriptExpectRuntimeException _) = "runtime exception"
    show ScriptExpectStop = "stop"
    show ScriptExpectSuccess = "success"

testerAssertThrowsException ::
    forall ex a.
    Exception ex =>
    (ex -> Bool) ->
    Tester a ->
    Tester ()
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
