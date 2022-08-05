module Main
    ( main
    ) where

import Data.IORef
import Data.Time
import Shapes
import Shapes.Test

baseTime :: UTCTime
baseTime = UTCTime (ModifiedJulianDay 0) 0

testFastClock :: TestTree
testFastClock =
    testTree "fast" $ do
        ref <- newIORef False
        runLifeCycleT $ do
            _ <-
                clock baseTime 0.1 $ \_ -> do
                    writeIORef ref True
                    threadDelay 1000000
                    writeIORef ref False
            liftIO $ threadDelay 500000
        bad <- readIORef ref
        if bad
            then assertFailure "bad async exception"
            else return ()

testSlowClock :: TestTree
testSlowClock =
    testTree "slow" $
    runLifeCycleT $ do
        _ <- clock baseTime (5000 * nominalDay) $ \_ -> return ()
        return ()

testClock :: TestTree
testClock = testTree "clock" [testFastClock, testSlowClock]

runFix ::
       forall m. (MonadIO m, MonadFix m)
    => m ()
runFix = do
    (_, x) <- mfixIO $ \(~(x, _)) -> return (True, x)
    liftIO $ assertEqual "" True x

runBoxes ::
       forall m. (MonadIO m, MonadFix m)
    => m ()
runBoxes = do
    refA <- liftIO $ newIORef 0
    let
        boxA :: FixBox m () ()
        boxA = let
            register :: Int -> m ()
            register x = liftIO $ writeIORef refA x
            construct :: () -> m (Int, ())
            construct () = return (3, ())
            in mkFixBox register construct
    refB <- liftIO $ newIORef 0
    let
        boxB :: FixBox m () ()
        boxB = let
            register :: Int -> m ()
            register x = liftIO $ writeIORef refB x
            construct :: () -> m (Int, ())
            construct () = return (5, ())
            in mkFixBox register construct
    refC <- liftIO $ newIORef 0
    let
        boxC :: FixBox m () ()
        boxC = let
            register :: Int -> m ()
            register x = liftIO $ writeIORef refC x
            construct :: () -> m (Int, ())
            construct () = do
                a <- liftIO $ readIORef refA
                b <- liftIO $ readIORef refB
                return (a + b, ())
            in mkFixBox register construct
    let
        boxUndefined :: FixBox m () ()
        boxUndefined = let
            register :: Int -> m ()
            register _ = return ()
            construct :: () -> m (Int, ())
            construct () = return (undefined, ())
            in mkFixBox register construct
    boxRecursiveIO (mconcat [boxA, boxUndefined, boxC, boxB]) ()
    x <- liftIO $ readIORef refC
    liftIO $ assertEqual "" 8 x

testFix :: TestTree
testFix =
    testTree "fix" [testTree "IO" (runFix :: IO ()), testTree "TransformT IO" (runTransformT runFix return :: IO ())]

testFixBox :: TestTree
testFixBox =
    testTree
        "fixbox"
        [testTree "IO" (runBoxes :: IO ()), testTree "TransformT IO" (runTransformT runBoxes return :: IO ())]

main :: IO ()
main = testMain $ testTree "shapes" [testClock, testFix, testFixBox]
