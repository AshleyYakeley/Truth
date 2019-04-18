{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Truth.Core

goldenTest :: TestName -> FilePath -> FilePath -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest name refPath outPath call =
    goldenVsFile name refPath outPath $
    withBinaryFile outPath WriteMode $ \h -> let
        ?handle = h
        in call

goldenTest' :: TestName -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest' name call = goldenTest name ("test/golden/" ++ name ++ ".ref") ("test/golden/" ++ name ++ ".out") call

{-
testEditor :: Editor (WholeEdit a) () a
testEditor = let
    editorInit (MkObject run r _) = run $ r ReadWhole
    editorUpdate _ _ _ = return ()
    editorDo editor _ = return editor
    in MkEditor {..}
-}
testSavable :: TestTree
testSavable = testCase "Savable" $ do return ()

{-
        object <- freeIOObject False (\_ -> True)
        sub <- makeObjectSubscriber object
        let
            saveSub = saveBufferSubscriber sub
            cleanSaveSub = fmap fst saveSub
        found <- subscribeEditor cleanSaveSub testEditor
        assertEqual "value" False found
-}
data SubscribeContext edit = MkSubscribeContext
    { subDoEdits :: [[edit]] -> IO ()
    , subDontEdits :: [[edit]] -> IO ()
    }

testOutputEditor ::
       forall edit. (Show edit, Show (EditSubject edit), FullSubjectReader (EditReader edit), ?handle :: Handle)
    => String
    -> (SubscribeContext edit -> IO ())
    -> Editor edit ()
testOutputEditor name call = let
    outputLn :: MonadIO m => String -> m ()
    outputLn s = liftIO $ hPutStrLn ?handle $ name ++ ": " ++ s
    editorInit :: Object edit -> IO ()
    editorInit (MkObject (MkTransform run) r _) = do
        val <- run $ mutableReadToSubject r
        outputLn $ "init: " ++ show val
        return ()
    editorUpdate :: () -> Object edit -> [edit] -> EditContext -> IO ()
    editorUpdate () (MkObject (MkTransform run) mr _) edits _ = do
        outputLn $ "receive " ++ show edits
        val <- run $ mutableReadToSubject mr
        outputLn $ "receive " ++ show val
    editorDo :: () -> Object edit -> IO ()
    editorDo () (MkObject (MkTransform run) _ push) = let
        subDontEdits :: [[edit]] -> IO ()
        subDontEdits editss = do
            outputLn "runObject"
            run $
                for_ editss $ \edits -> do
                    outputLn $ "push " ++ show edits
                    maction <- push edits
                    case maction of
                        Nothing -> outputLn "push disallowed"
                        Just _action -> outputLn "push ignored"
        subDoEdits :: [[edit]] -> IO ()
        subDoEdits editss = do
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
    -> ((?handle :: Handle, ?showVar :: IO (), ?showExpected :: [edit] -> IO ()) => Subscriber edit -> IO ())
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
        sub <- makeObjectSubscriber False editObj
        let
            ?showVar = withMVar var $ \s -> hPutStrLn ?handle $ "var: " ++ show s
            ?showExpected = \edits ->
                withMVar var $ \s -> do
                    news <- mutableReadToSubject $ applyEdits edits $ subjectToMutableRead s
                    hPutStrLn ?handle $ "expected: " ++ show news
        liftIO $ call sub

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
    testSubscription "SharedString1" "ABCDE" $ \sub -> do
        testLens <- stringSectionLens (startEndRun 1 4)
        subscribeEditor sub $
            testOutputEditor "main" $ \MkSubscribeContext {..} ->
                subscribeEditor (mapSubscriber testLens sub) $
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
    testSubscription "SharedString2" "ABC" $ \sub -> do
        testLens <- stringSectionLens (startEndRun 1 2)
        subscribeEditor sub $
            testOutputEditor "main" $ \_ ->
                subscribeEditor (mapSubscriber testLens sub) $
                testOutputEditor "lens" $ \MkSubscribeContext {..} -> do
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 0 0) "P"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 0 0) "Q"]]
                    ?showVar

testSharedString3 :: TestTree
testSharedString3 =
    testSubscription "SharedString3" "ABC" $ \sub -> do
        testLens <- stringSectionLens (startEndRun 1 2)
        subscribeEditor sub $
            testOutputEditor "main" $ \MkSubscribeContext {..} ->
                subscribeEditor (mapSubscriber testLens sub) $
                testOutputEditor "lens" $ \_ -> do
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 1 1) "P"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 2 2) "Q"]]
                    ?showVar

testSubscribe :: TestTree
testSubscribe =
    testGroup
        "subscribe"
        [ testSavable
        , testPair
        , testString
        , testString1
        , testString2
        , testSharedString1
        , testSharedString2
        , testSharedString3
        ]
