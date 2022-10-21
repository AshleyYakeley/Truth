module Shapes.Test
    (
    -- * General
      TestTree
    , TestName
    , getTestName
    , setTestName
    , repeatedTest
    , repeatTest
    , pauseTestOnFailure
    , testMain
    , testMainNoSignalHandler
    , BuildTestTree(..)
    , ignoreTestBecause
    , expectFailBecause
    , failTestBecause
    , testMark
    -- * Options
    , localOption
    , QuickCheckTests(..)
    , mkTimeout
    -- * Unit
    , Assertion
    , assertEqual
    , assertFailure
    , assertThrowsException
    , assertThrowsAnyException
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
import Shapes
import System.Directory
import System.Exit
import System.FilePath
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Providers.ConsoleFormat
import Test.Tasty.QuickCheck
import Test.Tasty.Runners

ingredientSetOptions :: (OptionSet -> OptionSet) -> Ingredient -> Ingredient
ingredientSetOptions f (TestReporter ods t) = TestReporter ods $ t . f
ingredientSetOptions f (TestManager ods t) = TestManager ods $ t . f

defaultTestIngredients :: [Ingredient]
defaultTestIngredients = fmap (ingredientSetOptions $ setOption $ AnsiTricks False) defaultIngredients

testMain :: TestTree -> IO ()
testMain = defaultMainWithIngredients defaultTestIngredients

testMainNoSignalHandler :: TestTree -> IO ()
testMainNoSignalHandler tests = do
    opts <- parseOptions defaultTestIngredients tests
    case tryIngredients defaultTestIngredients opts tests of
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

getTestName :: TestTree -> TestName
getTestName (SingleTest name _) = name
getTestName (TestGroup name _) = name
getTestName (PlusTestOptions _ tests) = getTestName tests
getTestName (WithResource _ ftests) = getTestName $ ftests $ fail "cannot get test name"
getTestName (AskOptions ftests) = getTestName $ ftests mempty
getTestName (After _ _ tests) = getTestName tests

setTestName :: TestName -> TestTree -> TestTree
setTestName name (SingleTest _ t) = SingleTest name t
setTestName name (TestGroup _ t) = TestGroup name t
setTestName name (PlusTestOptions options tests) = PlusTestOptions options $ setTestName name tests
setTestName name (WithResource rspec ftests) = WithResource rspec $ \ioa -> setTestName name $ ftests ioa
setTestName name (AskOptions ftests) = AskOptions $ \opts -> setTestName name $ ftests opts
setTestName name (After dtype expr tests) = After dtype expr $ setTestName name tests

repeatedTest :: Int -> TestTree -> TestTree
repeatedTest n tests = testGroup (getTestName tests) $ fmap (\i -> setTestName (show i <> "/" <> show n) tests) [1 .. n]

repeatTest :: Int -> TestTree -> TestTree
repeatTest n =
    wrapTest $ \test -> let
        go i
            | i > n =
                return $ let
                    resultOutcome = Test.Tasty.Runners.Success
                    resultDescription = ""
                    resultTime = 0
                    resultShortDescription = "OK"
                    resultDetailsPrinter = noResultDetails
                    in Result {..}
        go i = do
            r <- test
            case resultOutcome r of
                Test.Tasty.Runners.Success -> do
                    r' <- go (i + 1)
                    return r' {resultTime = resultTime r + resultTime r'}
                _ ->
                    return
                        r {resultShortDescription = resultShortDescription r <> " (" <> show i <> "/" <> show n <> ")"}
        in go 1

-- | time in ms
pauseTestOnFailure :: Int -> TestTree -> TestTree
pauseTestOnFailure t =
    wrapTest $ \action -> do
        r <- action
        case resultOutcome r of
            Test.Tasty.Runners.Failure _ -> do
                hPutStrLn stderr $ "pausing: " <> resultShortDescription r <> ": " <> resultDescription r
                threadDelay $ t * 1000
                hPutStrLn stderr "pause done"
            _ -> return ()
        return r

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

testMark :: TestTree -> TestTree
testMark test = testTree "MARK" [test]

testHandleVsFile :: FilePath -> TestName -> (Handle -> IO ()) -> TestTree
testHandleVsFile dir testName call = let
    refPath = dir </> testName <.> "ref"
    outPath = dir </> testName <.> "out"
    in goldenVsFile testName refPath outPath $ do
           createDirectoryIfMissing True dir
           withBinaryFile outPath WriteMode $ \h -> do
               hSetBuffering h NoBuffering
               call h

assertThrowsException ::
       forall ex a. Exception ex
    => (ex -> Bool)
    -> IO a
    -> IO ()
assertThrowsException checkEx ma = do
    result <- tryExc ma
    case result of
        SuccessResult _ -> assertFailure "no exception"
        FailureResult se ->
            case fromException se of
                Nothing -> assertFailure $ "bad exception type: " <> show se
                Just e ->
                    if checkEx e
                        then return ()
                        else assertFailure $ "bad exception: " <> show e

assertThrowsAnyException :: IO a -> IO ()
assertThrowsAnyException = assertThrowsException @SomeException $ \_ -> True
