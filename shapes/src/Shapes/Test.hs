module Shapes.Test
    (
    -- * General
      TestTree
    , TestName
    , testTreeNameLens
    , repeatTest
    , testMain
    , testMainNoSignalHandler
    , BuildTestTree(..)
    , ignoreTestBecause
    , expectFailBecause
    , failTestBecause
    -- * Options
    , localOption
    , QuickCheckTests(..)
    , mkTimeout
    -- * Unit
    , Assertion
    , assertEqual
    , assertFailure
    -- * QuickCheck
    , Testable
    , Property
    , Arbitrary(..)
    , oneof
    , getSmall
    , getNonNegative
    , getPrintableString
    , (===)
    , counterexample
    , ioProperty
    -- * Golden
    , testHandleVsFile
    , findByExtension
    , (</>)
    ) where

import GHC.IO.Handle
import Shapes hiding ((<.>))
import System.Directory
import System.Exit
import System.FilePath
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.Providers
import Test.Tasty.QuickCheck
import Test.Tasty.Runners

testMain :: TestTree -> IO ()
testMain = defaultMain

testMainNoSignalHandler :: TestTree -> IO ()
testMainNoSignalHandler tests = do
    opts <- parseOptions defaultIngredients tests
    case tryIngredients defaultIngredients opts tests of
        Nothing -> do
            hPutStrLn
                stderr
                "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
            exitFailure
        Just act -> do
            ok <- act
            if ok
                then exitSuccess
                else exitFailure

testTreeNameLens :: Lens' Identity TestTree TestName
testTreeNameLens = let
    lensGet :: TestTree -> TestName
    lensGet (SingleTest name _) = name
    lensGet (TestGroup name _) = name
    lensGet (PlusTestOptions _ tests) = lensGet tests
    lensGet (WithResource _ ftests) = lensGet $ ftests $ fail "cannot get test name"
    lensGet (AskOptions ftests) = lensGet $ ftests mempty
    lensGet (After _ _ tests) = lensGet tests
    lensPutback :: TestName -> TestTree -> Identity TestTree
    lensPutback name (SingleTest _ t) = Identity $ SingleTest name t
    lensPutback name (TestGroup _ t) = Identity $ TestGroup name t
    lensPutback name (PlusTestOptions options tests) =
        Identity $ PlusTestOptions options $ runIdentity $ lensPutback name tests
    lensPutback name (WithResource rspec ftests) =
        Identity $ WithResource rspec $ \ioa -> runIdentity $ lensPutback name $ ftests ioa
    lensPutback name (AskOptions ftests) = Identity $ AskOptions $ \opts -> runIdentity $ lensPutback name $ ftests opts
    lensPutback name (After dtype expr tests) = Identity $ After dtype expr $ runIdentity $ lensPutback name tests
    in MkLens {..}

repeatTest :: Int -> TestTree -> TestTree
repeatTest n tests =
    testGroup (lensGet testTreeNameLens tests) $
    fmap (\i -> runIdentity $ lensPutback testTreeNameLens (show i <> "/" <> show n) tests) [1 .. n]

failTestBecause :: String -> TestTree -> TestTree
failTestBecause reason = wrapTest $ \_ -> return $ (testFailed "") {resultShortDescription = "FAILS: " <> reason}

class BuildTestTree a where
    testTree :: TestName -> a -> TestTree

instance BuildTestTree [TestTree] where
    testTree = testGroup

instance BuildTestTree Assertion where
    testTree = Test.Tasty.HUnit.testCase

instance BuildTestTree Property where
    testTree = testProperty

instance (Arbitrary a, Show a, Testable b) => BuildTestTree (a -> b) where
    testTree name = testTree name . property

testHandleVsFile :: FilePath -> TestName -> (Handle -> IO ()) -> TestTree
testHandleVsFile dir testName call = let
    refPath = dir </> testName <.> "ref"
    outPath = dir </> testName <.> "out"
    in goldenVsFile testName refPath outPath $ do
           createDirectoryIfMissing True dir
           withBinaryFile outPath WriteMode $ \h -> do
               hSetBuffering h NoBuffering
               call h
