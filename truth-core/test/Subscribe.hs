{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.Golden
import Truth.Core

goldenTest :: TestName -> FilePath -> FilePath -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest name refPath outPath call =
    goldenVsFile name refPath outPath $
    withBinaryFile outPath WriteMode $ \h -> let
        ?handle = h
        in call

goldenTest' :: TestName -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest' name call = goldenTest name ("test/golden/" ++ name ++ ".ref") ("test/golden/" ++ name ++ ".out") call

data SubscribeContext edit = MkSubscribeContext
    { subDoEdits :: [[edit]] -> LifeCycleIO ()
    , subDontEdits :: [[edit]] -> LifeCycleIO ()
    }

testOutputEditor ::
       forall edit. (Show edit, Show (EditSubject edit), FullSubjectReader (EditReader edit), ?handle :: Handle)
    => String
    -> (SubscribeContext edit -> LifeCycleIO ())
    -> Editor edit ()
testOutputEditor name call = let
    outputLn :: MonadIO m => String -> m ()
    outputLn s = liftIO $ hPutStrLn ?handle $ name ++ ": " ++ s
    editorInit :: Object edit -> LifeCycleIO ()
    editorInit (MkCloseUnliftIO (MkTransform run) (MkAnObject r _)) =
        liftIO $ do
            val <- run $ mutableReadToSubject r
            outputLn $ "init: " ++ show val
            return ()
    editorUpdate :: () -> Object edit -> [edit] -> EditContext -> IO ()
    editorUpdate () (MkCloseUnliftIO (MkTransform run) (MkAnObject mr _)) edits _ = do
        outputLn $ "receive " ++ show edits
        val <- run $ mutableReadToSubject mr
        outputLn $ "receive " ++ show val
    editorDo :: () -> Object edit -> LifeCycleIO ()
    editorDo () (MkCloseUnliftIO (MkTransform run) (MkAnObject _ push)) = let
        subDontEdits :: [[edit]] -> LifeCycleIO ()
        subDontEdits editss =
            liftIO $ do
                outputLn "runObject"
                run $
                    for_ editss $ \edits -> do
                        outputLn $ "push " ++ show edits
                        maction <- push edits
                        case maction of
                            Nothing -> outputLn "push disallowed"
                            Just _action -> outputLn "push ignored"
        subDoEdits :: [[edit]] -> LifeCycleIO ()
        subDoEdits editss =
            liftIO $ do
                outputLn "runObject"
                run $
                    for_ editss $ \edits -> do
                        outputLn $ "push " ++ show edits
                        maction <- push edits
                        case maction of
                            Nothing -> outputLn "push disallowed"
                            Just action -> do
                                action noEditSource
                                outputLn $ "push succeeded"
        in call MkSubscribeContext {..}
    in MkEditor {..}

testSubscription ::
       forall edit. (FullEdit edit, Show (EditSubject edit))
    => TestName
    -> EditSubject edit
    -> ((?handle :: Handle, ?showVar :: LifeCycleIO (), ?showExpected :: [edit] -> LifeCycleIO ()) =>
                Subscriber edit -> LifeCycleIO ())
    -> TestTree
testSubscription name initial call =
    goldenTest' name $
    runLifeCycle $ do
        var <- liftIO $ newMVar initial
        let
            varObj :: Object (WholeEdit (EditSubject edit))
            varObj = mvarObject var $ \_ -> True
            editObj :: Object edit
            editObj = convertObject varObj
        sub <- makeReflectingSubscriber SynchronousUpdateTiming editObj
        let
            ?showVar = liftIO $ withMVar var $ \s -> hPutStrLn ?handle $ "var: " ++ show s
            ?showExpected = \edits ->
                liftIO $
                withMVar var $ \s -> do
                    news <- mutableReadToSubject $ applyEdits edits $ subjectToMutableRead s
                    hPutStrLn ?handle $ "expected: " ++ show news
        call sub

testPair :: TestTree
testPair =
    testSubscription @(PairEdit (WholeEdit Bool) (WholeEdit Bool)) "Pair" (False, False) $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            ?showExpected [MkTupleEdit SelectFirst $ MkWholeEdit True, MkTupleEdit SelectSecond $ MkWholeEdit True]
            subDoEdits [[MkTupleEdit SelectFirst $ MkWholeEdit True, MkTupleEdit SelectSecond $ MkWholeEdit True]]
            ?showVar

testString :: TestTree
testString =
    testSubscription "String" "ABCDE" $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]]
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 2 3) ""]]
            ?showVar
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]]
            ?showVar
            subDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]]
            ?showVar

testString1 :: TestTree
testString1 =
    testSubscription "String1" "ABCDE" $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"], [StringReplaceSection (startEndRun 2 3) ""]]
            ?showVar
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"], [StringReplaceSection (startEndRun 2 4) "1"]]
            ?showVar

testString2 :: TestTree
testString2 =
    testSubscription "String2" "ABCDE" $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR", StringReplaceSection (startEndRun 2 3) ""]]
            ?showVar
            ?showExpected [StringReplaceSection (startEndRun 1 2) "xy", StringReplaceSection (startEndRun 2 4) "1"]
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy", StringReplaceSection (startEndRun 2 4) "1"]]
            ?showVar

testSharedString1 :: TestTree
testSharedString1 =
    testSubscription "SharedString1" "ABCDE" $ \mainSub -> do
        testLens <- liftIO $ stringSectionLens (startEndRun 1 4)
        subscribeEditor mainSub $
            testOutputEditor "main" $ \MkSubscribeContext {..} -> do
                lensSub <- mapSubscriber testLens mainSub
                subscribeEditor lensSub $
                    testOutputEditor "lens" $ \_ -> do
                        ?showVar
                        subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]]
                        ?showVar
                        subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]]
                        ?showVar
                        subDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]]
                        ?showVar

testSharedString2 :: TestTree
testSharedString2 =
    testSubscription "SharedString2" "ABC" $ \mainSub -> do
        testLens <- liftIO $ stringSectionLens (startEndRun 1 2)
        subscribeEditor mainSub $
            testOutputEditor "main" $ \_ -> do
                lensSub <- mapSubscriber testLens mainSub
                subscribeEditor lensSub $
                    testOutputEditor "lens" $ \MkSubscribeContext {..} -> do
                        ?showVar
                        subDoEdits [[StringReplaceSection (startEndRun 0 0) "P"]]
                        ?showVar
                        subDoEdits [[StringReplaceSection (startEndRun 0 0) "Q"]]
                        ?showVar

testSharedString3 :: TestTree
testSharedString3 =
    testSubscription "SharedString3" "ABC" $ \mainSub -> do
        testLens <- liftIO $ stringSectionLens (startEndRun 1 2)
        subscribeEditor mainSub $
            testOutputEditor "main" $ \MkSubscribeContext {..} -> do
                lensSub <- mapSubscriber testLens mainSub
                subscribeEditor lensSub $ pure ()
                subscribeEditor lensSub $
                    testOutputEditor "lens" $ \_ -> do
                        ?showVar
                        subDoEdits [[StringReplaceSection (startEndRun 1 1) "P"]]
                        ?showVar
                        subDoEdits [[StringReplaceSection (startEndRun 2 2) "Q"]]
                        ?showVar

testSharedString4 :: TestTree
testSharedString4 =
    testSubscription "SharedString4" "ABC" $ \mainSub -> do
        testLens <- liftIO $ stringSectionLens (startEndRun 1 2)
        subscribeEditor mainSub $
            testOutputEditor "main" $ \main -> do
                lensSub <- mapSubscriber testLens mainSub
                subscribeEditor lensSub $ pure ()
                subscribeEditor lensSub $
                    testOutputEditor "lens" $ \sect -> do
                        ?showVar
                        subDoEdits main [[StringReplaceSection (startEndRun 0 0) "P"]]
                        ?showVar
                        subDoEdits sect [[StringReplaceSection (startEndRun 0 0) "Q"]]
                        ?showVar

testSharedString5 :: TestTree
testSharedString5 =
    testSubscription "SharedString5" "ABCD" $ \mainSub -> do
        testLens <- liftIO $ stringSectionLens (startEndRun 1 3)
        subscribeEditor mainSub $
            testOutputEditor "main" $ \main -> do
                lensSub <- mapSubscriber testLens mainSub
                subscribeEditor lensSub $
                    testOutputEditor "lens" $ \_sect -> do
                        ?showVar
                        subDoEdits main [[StringReplaceSection (startEndRun 2 4) ""]]
                        ?showVar

testSharedString6 :: TestTree
testSharedString6 =
    testSubscription "SharedString6" "ABCD" $ \mainSub -> do
        testLens <- liftIO $ stringSectionLens (startEndRun 1 3)
        subscribeEditor mainSub $
            testOutputEditor "main" $ \main -> do
                lensSub <- mapSubscriber testLens mainSub
                subscribeEditor lensSub $
                    testOutputEditor "lens" $ \_sect -> do
                        ?showVar
                        subDoEdits main [[StringReplaceSection (startEndRun 3 4) ""]]
                        ?showVar

testSharedString7 :: TestTree
testSharedString7 =
    testSubscription "SharedString7" "ABCD" $ \mainSub -> do
        testLens <- liftIO $ stringSectionLens (startEndRun 1 3)
        subscribeEditor mainSub $
            testOutputEditor "main" $ \main -> do
                lensSub <- mapSubscriber testLens mainSub
                subscribeEditor lensSub $
                    testOutputEditor "lens" $ \_sect -> do
                        ?showVar
                        subDoEdits main [[StringReplaceSection (startEndRun 2 4) "PQR"]]
                        ?showVar

testSubscribe :: TestTree
testSubscribe =
    testGroup
        "subscribe"
        [ testPair
        , testString
        , testString1
        , testString2
        , testSharedString1
        , testSharedString2
        , testSharedString3
        , testSharedString4
        , testSharedString5
        , testSharedString6
        , testSharedString7
        ]
