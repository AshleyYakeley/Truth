module Main
    ( main
    ) where

import Control.Applicative
import Control.Monad.Ology
import Data.IORef
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

testComposeInnerApplicative :: TestTree
testComposeInnerApplicative =
    testCase "Applicative" $ do
        r1 <- newIORef False
        r2 <- newIORef False
        let
            c1 :: ComposeInner Maybe IO ()
            c1 = do
                lift $ writeIORef r1 True
                liftInner Nothing
            c2 :: ComposeInner Maybe IO ()
            c2 = lift $ writeIORef r2 True
        _ <- getComposeInner $ liftA2 (,) c1 c2
        v1 <- readIORef r1
        v2 <- readIORef r2
        assertEqual "v1" True v1
        assertEqual "v2" False v2

testComposeInnerAlternative :: TestTree
testComposeInnerAlternative =
    testCase "Alternative" $ do
        r1 <- newIORef False
        r2 <- newIORef False
        let
            c1 :: ComposeInner Maybe IO ()
            c1 = lift $ writeIORef r1 True
            c2 :: ComposeInner Maybe IO ()
            c2 = lift $ writeIORef r2 True
        _ <- getComposeInner $ c1 <|> c2
        v1 <- readIORef r1
        v2 <- readIORef r2
        assertEqual "v1" True v1
        assertEqual "v2" False v2

testComposeInner :: TestTree
testComposeInner = testGroup "composeInner" [testComposeInnerApplicative, testComposeInnerAlternative]

testACatch ::
       forall m e. (Eq e, Show e, MonadCatch e m)
    => m --> IO -> e -> TestTree
testACatch runM ex =
    testCase "catchExc" $ do
        r <- runM $ try @m @e $ throw @e @m @() ex
        assertEqual "caught" (FailureResult ex) r

testABracket ::
       forall m. (MonadTunnelIO m, MonadException m)
    => m --> IO -> (forall a. m a) -> TestTree
testABracket runM th =
    testCase "bracket" $ do
        ref <- newIORef False
        _ <- tryExc $ runM $ finally @m th (liftIO $ writeIORef ref True)
        b <- readIORef ref
        assertEqual "finally" True b

testAnException ::
       forall m e. (Eq e, Show e, MonadException m, MonadCatch e m, MonadTunnelIO m)
    => String
    -> m --> IO -> e -> TestTree
testAnException name runM ex = testGroup name [testACatch runM ex, testABracket runM $ throw ex]

runComposeInner :: ComposeInner Maybe IO --> IO
runComposeInner (MkComposeInner ima) = do
    ma <- ima
    case ma of
        Just a -> return a
        Nothing -> fail "Nothing"

runExceptT' :: ExceptT () IO --> IO
runExceptT' eia = do
    ea <- runExceptT eia
    case ea of
        Right a -> return a
        Left _ -> fail "Left"

testException :: TestTree
testException =
    testGroup
        "Exception"
        [ testAnException "IO" id $ ErrorCall "test"
        , testGroup "ExceptT () IO" $ pure $ testABracket @(ExceptT () IO) runExceptT' $ throwE ()
        , testGroup "ComposeInner Maybe IO" $
          pure $ testABracket @(ComposeInner Maybe IO) runComposeInner $ liftInner Nothing
        ]

compareTest :: String -> ((String -> IO ()) -> IO r) -> IO r
compareTest expected action = do
    resultsRef <- newIORef ""
    let
        appendStr :: String -> IO ()
        appendStr s = modifyIORef resultsRef $ \t -> t ++ s
    r <- action appendStr
    found <- readIORef resultsRef
    assertEqual "" expected found
    return r

withMessage :: (String -> IO ()) -> String -> IO r -> IO r
withMessage appendStr s m = do
    appendStr $ "+" <> s
    r <- m
    appendStr $ "-" <> s
    return r

testCoroutine :: TestTree
testCoroutine =
    testCase "coroutine" $
    compareTest "+A+B-A-B" $ \appendStr -> do
        _ <-
            coroutine
                (\y ->
                     withMessage appendStr "A" $ do
                         y ()
                         return ((), ()))
                (\() y ->
                     withMessage appendStr "B" $ do
                         y ()
                         return ((), ()))
        return ()

testLifeCycle :: TestTree
testLifeCycle =
    testCase "lifecycle" $
    compareTest "ACDB" $ \appendStr -> do
        let
            lc :: LifeCycle ()
            lc = do
                liftIO $ appendStr "A"
                lifeCycleOnClose $ appendStr "B"
                liftIO $ appendStr "C"
                lifeCycleOnClose $ appendStr "D"
        runLifeCycleT lc

tests :: TestTree
tests = testGroup "monadology" [testCoroutine, testLifeCycle, testComposeInner, testException]

main :: IO ()
main = defaultMain tests
