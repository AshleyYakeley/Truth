module Shapes.Test
    (
    -- * General
      TestTree
    , TestName
    , testMain
    , BuildTestTree(..)
    , ignoreTest
    , expectFail
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
    , testOutputVsFile
    , findByExtension
    , (</>)
    ) where

import Shapes hiding ((<.>))
import System.Directory
import System.FilePath
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testMain :: TestTree -> IO ()
testMain = defaultMain

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

testOutputVsFile :: FilePath -> TestName -> (Handle -> IO ()) -> TestTree
testOutputVsFile dir testName call = let
    refPath = dir </> testName <.> "ref"
    outPath = dir </> testName <.> "out"
    in goldenVsFile testName refPath outPath $ do
           createDirectoryIfMissing True dir
           withBinaryFile outPath WriteMode $ \h -> do
               hSetBuffering h NoBuffering
               call h
