module Test.Context where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

type ContextTestTree = [String] -> TestTree

tmodify :: (TestTree -> TestTree) -> ContextTestTree -> ContextTestTree
tmodify f ct c = f $ ct c

context :: [String] -> ContextTestTree -> ContextTestTree
context defs tree c = tree $ defs <> c

tgroup :: String -> [ContextTestTree] -> ContextTestTree
tgroup name tests c = testGroup name $ fmap (\test -> test c) tests

runContext :: ContextTestTree -> TestTree
runContext tree = tree mempty

prefix :: [String] -> Text
prefix c = pack $ "let\n" ++ intercalate ";\n" c ++ "\nin\n"

contextTestCase :: Text -> Text -> (Text -> IO ()) -> ContextTestTree
contextTestCase name text tester c = testCase (unpack name) $ tester $ prefix c <> text

contextTestProperty :: Testable prop => Text -> Text -> (Text -> prop) -> ContextTestTree
contextTestProperty name text prop c = testProperty (unpack name) $ prop $ prefix c <> text

message :: MonadIO m => String -> m ()
message s = liftIO $ hPutStrLn stderr s
