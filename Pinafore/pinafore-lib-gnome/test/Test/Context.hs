module Test.Context
    ( module Shapes.Test
    , module Shapes.Test.Context
    , ScriptTestTree
    , runScriptTestTree
    , tPrefix
    , tDeclarator
    , tDecls
    , tDeclsRec
    , tWith
    , tImport
    , scriptTestCase
    ) where

import Shapes
import Shapes.Test
import Shapes.Test.Context

type ScriptTestTree = ContextTestTree Text

runScriptTestTree :: ScriptTestTree -> TestTree
runScriptTestTree = runContextTestTree mempty

tPrefix :: Text -> ScriptTestTree -> ScriptTestTree
tPrefix p = tContext $ \t -> p <> t

tDeclarator :: Text -> ScriptTestTree -> ScriptTestTree
tDeclarator t = tPrefix $ t <> "\n"

tWith :: [Text] -> ScriptTestTree -> ScriptTestTree
tWith tt = tDeclarator $ "with " <> intercalate ", " tt

tImport :: [Text] -> ScriptTestTree -> ScriptTestTree
tImport tt = tDeclarator $ "import " <> intercalate ", " (fmap (pack . show) tt)

tDecls :: [String] -> ScriptTestTree -> ScriptTestTree
tDecls defs = tPrefix $ pack $ "let {\n" <> intercalate ";\n" defs <> "\n}\n"

tDeclsRec :: [String] -> ScriptTestTree -> ScriptTestTree
tDeclsRec defs = tPrefix $ pack $ "let rec {\n" <> intercalate ";\n" defs <> "\n}\n"

scriptTestCase :: Text -> Text -> (Text -> IO ()) -> ScriptTestTree
scriptTestCase name text tester = MkContextTestTree $ \t -> testTree (unpack name) $ tester $ t <> text
