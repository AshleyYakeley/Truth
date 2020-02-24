module Resource
    ( testResource
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Useful
import Truth.Core

data AThing (tt :: [TransKind]) =
    MkAThing (ApplyStack tt IO ())

instance MapResource AThing where
    mapResource f (MkAThing mu) = MkAThing $ tlfFunction f (Proxy @IO) mu

type Thing = Resource AThing

joinThings :: Thing -> Thing -> Thing
joinThings =
    joinResource $ \(MkAThing m1 :: _ tt) (MkAThing m2) ->
        case transStackDict @Monad @tt @IO of
            Dict -> MkAThing $ m1 >> m2

stateResourceRunnerTrace :: (?handle :: Handle) => String -> s -> IO (ResourceRunner '[ StateT s])
stateResourceRunnerTrace name s = do
    var <- newMVar s
    newResourceRunner $ \ma -> do
        liftIO $ hPutStrLn ?handle $ name <> ": outside ["
        a <-
            mVarRun var $ do
                liftIO $ hPutStrLn ?handle $ name <> ": inside ["
                a <- ma
                liftIO $ hPutStrLn ?handle $ name <> ": inside ]"
                return a
        liftIO $ hPutStrLn ?handle $ name <> ": outside ]"
        return a

simpleThing :: (?handle :: Handle) => String -> IO Thing
simpleThing name = do
    rr <- stateResourceRunnerTrace name (0 :: Int)
    return $
        MkResource rr $
        MkAThing $ do
            i <- get
            lift $ hPutStrLn ?handle $ name <> ": " <> show (succ i)
            put $ succ i

testIOWitness :: TestTree
testIOWitness =
    testCase "iowitness" $ do
        a <- newIOWitness
        case testEquality a a of
            Just Refl -> return ()
            Nothing -> fail "non-reflexive"

testUnlift :: TestTree
testUnlift =
    testCase "unlift" $ do
        var <- newMVar ()
        mVarRun var $ liftWithUnlift $ \unlift -> unlift $ return ()

testResource1 :: TestTree
testResource1 =
    goldenTest "resource" "1" $ do
        thingA <- simpleThing "A"
        runResource emptyResourceContext thingA $ \(MkAThing ioa) -> ioa
        return ()

testResource1c :: TestTree
testResource1c =
    goldenTest "resource" "1c" $ do
        thingA <- simpleThing "A"
        runResourceContext emptyResourceContext thingA $ \_rc1 unlift (MkAThing ioa) -> unlift ioa
        return ()

testResource2 :: TestTree
testResource2 =
    goldenTest "resource" "2" $ do
        thingA <- simpleThing "A"
        runResourceContext emptyResourceContext thingA $ \rc1 _ _ -> runResource rc1 thingA $ \(MkAThing ioa) -> ioa
        return ()

testResource2c :: TestTree
testResource2c =
    goldenTest "resource" "2c" $ do
        thingA <- simpleThing "A"
        runResourceContext emptyResourceContext thingA $ \rc1 _ _ ->
            runResourceContext rc1 thingA $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testDoubled :: TestTree
testDoubled =
    goldenTest "resource" "Doubled" $ do
        thingA <- simpleThing "A"
        let thingAA = joinThings thingA thingA
        runResource emptyResourceContext thingAA $ \(MkAThing ioa) -> ioa
        return ()

testOneDoubled :: TestTree
testOneDoubled =
    goldenTest "resource" "OneDoubled" $ do
        thingA <- simpleThing "A"
        let thingAA = joinThings thingA thingA
        runResourceContext emptyResourceContext thingA $ \rc1 _ _ ->
            runResourceContext rc1 thingAA $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testDoubledOne :: TestTree
testDoubledOne =
    goldenTest "resource" "DoubledOne" $ do
        thingA <- simpleThing "A"
        let thingAA = joinThings thingA thingA
        runResourceContext emptyResourceContext thingAA $ \rc1 _ _ ->
            runResourceContext rc1 thingA $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testOneOne :: TestTree
testOneOne =
    goldenTest "resource" "OneOne" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        runResourceContext emptyResourceContext thingA $ \rc1 _ _ ->
            runResourceContext rc1 thingB $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testPairOneA :: TestTree
testPairOneA =
    goldenTest "resource" "PairOneA" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        let thingAB = joinThings thingA thingB
        runResourceContext emptyResourceContext thingAB $ \rc1 _ _ ->
            runResourceContext rc1 thingA $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testPairOneB :: TestTree
testPairOneB =
    goldenTest "resource" "PairOneB" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        let thingAB = joinThings thingA thingB
        runResourceContext emptyResourceContext thingAB $ \rc1 _ _ ->
            runResourceContext rc1 thingB $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testOneAPair :: TestTree
testOneAPair =
    goldenTest "resource" "OneAPair" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        let thingAB = joinThings thingA thingB
        runResourceContext emptyResourceContext thingA $ \rc1 _ _ ->
            runResourceContext rc1 thingAB $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testOneBPair :: TestTree
testOneBPair =
    goldenTest "resource" "OneBPair" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        let thingAB = joinThings thingA thingB
        runResourceContext emptyResourceContext thingB $ \rc1 _ _ ->
            runResourceContext rc1 thingAB $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testPairPair :: TestTree
testPairPair =
    goldenTest "resource" "PairPair" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        let thingAB = joinThings thingA thingB
        runResourceContext emptyResourceContext thingAB $ \rc1 _ _ ->
            runResourceContext rc1 thingAB $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testPairSwap :: TestTree
testPairSwap =
    goldenTest "resource" "PairSwap" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        let
            thingAB = joinThings thingA thingB
            thingBA = joinThings thingB thingA
        runResourceContext emptyResourceContext thingAB $ \rc1 _ _ ->
            runResourceContext rc1 thingBA $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testABBC :: TestTree
testABBC =
    goldenTest "resource" "ABBC" $ do
        thingA <- simpleThing "A"
        thingB <- simpleThing "B"
        thingC <- simpleThing "C"
        let
            thingAB = joinThings thingA thingB
            thingBC = joinThings thingB thingC
        runResourceContext emptyResourceContext thingAB $ \rc1 _ _ ->
            runResourceContext rc1 thingBC $ \_rc2 unlift (MkAThing ioa) -> unlift ioa
        return ()

testResource :: TestTree
testResource =
    testGroup
        "resource"
        [ testIOWitness
        , testUnlift
        , testResource1
        , testResource1c
        , testResource2
        , testResource2c
        , testDoubled
        , testOneDoubled
        , testDoubledOne
        , testOneOne
        , testPairOneA
        , testPairOneB
        , testOneAPair
        , testOneBPair
        , testPairPair
        , testPairSwap
        , testABBC
        ]
