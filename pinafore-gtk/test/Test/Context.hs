module Test.Context
    ( module Shapes.Test
    , module Shapes.Test.Context
    , ScriptTestTree
    , runScriptTestTree
    , tDecls
    , tDeclsRec
    , scriptTestCase
    ) where

import Shapes
import Shapes.Test
import Shapes.Test.Context

type ScriptTestTree = ContextTestTree [String]

runScriptTestTree :: ScriptTestTree -> TestTree
runScriptTestTree = runContextTestTree mempty

tDecls :: [String] -> ScriptTestTree -> ScriptTestTree
tDecls defs = tContext $ \c -> defs <> c

tDeclsRec :: [String] -> ScriptTestTree -> ScriptTestTree
tDeclsRec defs = tDecls $ pure $ "rec\n" ++ intercalate ";\n" defs ++ "\nend"

prefix :: [String] -> Text
prefix c = pack $ "let\n" ++ intercalate ";\n" c ++ "\nin\n"

scriptTestCase :: Text -> Text -> (Text -> IO ()) -> ScriptTestTree
scriptTestCase name text tester = MkContextTestTree $ \c -> testTree (unpack name) $ tester $ prefix c <> text
