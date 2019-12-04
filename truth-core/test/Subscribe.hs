{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.Golden
import Truth.Core

debugLens ::
       forall updateA updateB.
       (Show updateA, Show updateB, Show (UpdateEdit updateA), Show (UpdateEdit updateB), ?handle :: Handle)
    => String
    -> EditLens updateA updateB
    -> EditLens updateA updateB
debugLens name (MkEditLens (MkUpdateFunction g u) pe) = let
    u' :: forall m. MonadIO m
       => updateA
       -> MutableRead m (UpdateReader updateA)
       -> m [updateB]
    u' ua mr = do
        liftIO $ hPutStrLn ?handle $ name ++ ": +update: " ++ show ua
        ubs <- u ua mr
        liftIO $ hPutStrLn ?handle $ name ++ ": -update: " ++ show ubs
        return ubs
    pe' :: forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    pe' ebs mr = do
        liftIO $ hPutStrLn ?handle $ name ++ ": +put: " ++ show ebs
        meas <- pe ebs mr
        liftIO $ hPutStrLn ?handle $ name ++ ": -put: " ++ show meas
        return meas
    in MkEditLens (MkUpdateFunction g u') pe'

goldenTest :: TestName -> FilePath -> FilePath -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest name refPath outPath call =
    goldenVsFile name refPath outPath $
    withBinaryFile outPath WriteMode $ \h -> let
        ?handle = h
        in call

goldenTest' :: TestName -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest' name call = goldenTest name ("test/golden/" ++ name ++ ".ref") ("test/golden/" ++ name ++ ".out") call

data SubscribeContext edit = MkSubscribeContext
    { subGet :: LifeCycleIO (EditSubject edit)
    , subDoEdits :: [[edit]] -> LifeCycleIO ()
    , subDontEdits :: [[edit]] -> LifeCycleIO ()
    }

testUpdateFunction ::
       forall a. (?handle :: Handle, Show a)
    => UpdateFunction (WholeUpdate a) (WholeUpdate a)
testUpdateFunction = let
    ufGet :: ReadFunction (WholeReader a) (WholeReader a)
    ufGet mr = mr
    ufUpdate ::
           forall m. MonadIO m
        => WholeUpdate a
        -> MutableRead m (WholeReader a)
        -> m [WholeUpdate a]
    ufUpdate (MkWholeReaderUpdate s) mr = do
        s' <- mr ReadWhole
        liftIO $ hPutStrLn ?handle $ "lens update edit: " <> show s
        liftIO $ hPutStrLn ?handle $ "lens update MR: " <> show s'
        return [MkWholeReaderUpdate s]
    in MkUpdateFunction {..}

testUpdateObject :: TestTree
testUpdateObject =
    goldenTest' "updateObject" $ do
        obj <- freeIOObject "old" $ \_ -> True
        let
            om :: ObjectMaker (WholeUpdate String) ()
            om = reflectingObjectMaker obj
            lens :: EditLens (WholeUpdate String) (WholeUpdate String)
            lens = readOnlyEditLens testUpdateFunction
            recv :: [WholeUpdate String] -> EditContext -> IO ()
            recv ee _ = for_ ee $ \(MkWholeReaderUpdate s) -> hPutStrLn ?handle $ "recv update edit: " <> show s
            recv' :: [WholeUpdate String] -> EditContext -> IO ()
            recv' ee _ = for_ ee $ \(MkWholeReaderUpdate s) -> hPutStrLn ?handle $ "recv' update edit: " <> show s
        runLifeCycle $ do
            om' <- shareObjectMaker om
            (MkResource1 trun MkAnObject {..}, ()) <- om' recv
            (_obj', ()) <- mapObjectMaker lens om' recv'
            runResourceRunnerWith trun $ \run ->
                liftIO $ run $ do pushOrFail "failed" noEditSource $ objEdit [MkWholeReaderEdit "new"]
            return ()

testOutputEditor ::
       forall update.
       ( Show update
       , Show (UpdateEdit update)
       , Show (UpdateSubject update)
       , FullSubjectReader (UpdateReader update)
       , ?handle :: Handle
       )
    => String
    -> (SubscribeContext (UpdateEdit update) -> LifeCycleIO ())
    -> Editor update ()
testOutputEditor name call = let
    outputLn :: MonadIO m => String -> m ()
    outputLn s = liftIO $ hPutStrLn ?handle $ name ++ ": " ++ s
    editorInit :: Object (UpdateEdit update) -> LifeCycleIO ()
    editorInit (MkResource1 trun (MkAnObject r _)) =
        runResourceRunnerWith trun $ \run ->
            liftIO $ do
                val <- run $ mutableReadToSubject r
                outputLn $ "init: " ++ show val
                return ()
    editorUpdate :: () -> Object (UpdateEdit update) -> [update] -> EditContext -> IO ()
    editorUpdate () (MkResource1 trun (MkAnObject mr _)) edits _ =
        runResourceRunnerWith trun $ \run -> do
            outputLn $ "receive " ++ show edits
            val <- run $ mutableReadToSubject mr
            outputLn $ "receive " ++ show val
    editorDo :: () -> Object (UpdateEdit update) -> LifeCycleIO ()
    editorDo () (MkResource1 trun (MkAnObject mr push)) =
        runResourceRunnerWith trun $ \run -> let
            subGet :: LifeCycleIO (UpdateSubject update)
            subGet =
                liftIO $ do
                    outputLn "runObject"
                    run $ do
                        val <- mutableReadToSubject mr
                        outputLn $ "get " ++ show val
                        return val
            subDontEdits :: [[UpdateEdit update]] -> LifeCycleIO ()
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
            subDoEdits :: [[UpdateEdit update]] -> LifeCycleIO ()
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
       forall update. (IsUpdate update, FullEdit (UpdateEdit update), Show (UpdateSubject update))
    => TestName
    -> UpdateSubject update
    -> ((?handle :: Handle, ?showVar :: LifeCycleIO (), ?showExpected :: [UpdateEdit update] -> LifeCycleIO ()) =>
                Subscriber update -> LifeCycleIO ())
    -> TestTree
testSubscription name initial call =
    goldenTest' name $
    runLifeCycle $ do
        iow <- liftIO $ newIOWitness
        var <- liftIO $ newMVar initial
        let
            varObj :: Object (WholeEdit (UpdateSubject update))
            varObj = mvarObject iow var $ \_ -> True
            editObj :: Object (UpdateEdit update)
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
    testSubscription @(PairUpdate (WholeUpdate Bool) (WholeUpdate Bool)) "Pair" (False, False) $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            ?showExpected
                [ MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True
                , MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True
                ]
            subDoEdits
                [ [ MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True
                  , MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True
                  ]
                ]
            ?showVar

testString :: TestTree
testString =
    testSubscription @(StringUpdate String) "String" "ABCDE" $ \sub ->
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
    testSubscription @(StringUpdate String) "String1" "ABCDE" $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"], [StringReplaceSection (startEndRun 2 3) ""]]
            ?showVar
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"], [StringReplaceSection (startEndRun 2 4) "1"]]
            ?showVar

testString2 :: TestTree
testString2 =
    testSubscription @(StringUpdate String) "String2" "ABCDE" $ \sub ->
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
    testSubscription @(StringUpdate String) "SharedString1" "ABCDE" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 4)) mainSub
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \_ -> do
                    ?showVar
                    subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]]
                    ?showVar

testSharedString2 :: TestTree
testSharedString2 =
    testSubscription @(StringUpdate String) "SharedString2" "ABC" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \_ -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \MkSubscribeContext {..} -> do
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 0 0) "P"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 0 0) "Q"]]
                    ?showVar

testSharedString3 :: TestTree
testSharedString3 =
    testSubscription @(StringUpdate String) "SharedString3" "ABC" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
            subscribeEditor lensSub $ pure ()
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \_ -> do
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 1 1) "P"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 2 2) "Q"]]
                    ?showVar

testSharedString4 :: TestTree
testSharedString4 =
    testSubscription @(StringUpdate String) "SharedString4" "ABC" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \main -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
            subscribeEditor lensSub $ pure ()
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \sect -> do
                    ?showVar
                    subDoEdits main [[StringReplaceSection (startEndRun 0 0) "P"]]
                    _ <- subGet sect
                    ?showVar
                    subDoEdits sect [[StringReplaceSection (startEndRun 0 0) "Q"]]
                    ?showVar

testSharedString5 :: TestTree
testSharedString5 =
    testSubscription @(StringUpdate String) "SharedString5" "ABCD" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \main -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 3)) mainSub
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \_sect -> do
                    ?showVar
                    subDoEdits main [[StringReplaceSection (startEndRun 2 4) ""]]
                    ?showVar

testSharedString6 :: TestTree
testSharedString6 =
    testSubscription @(StringUpdate String) "SharedString6" "ABCD" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \main -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 3)) mainSub
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \_sect -> do
                    ?showVar
                    subDoEdits main [[StringReplaceSection (startEndRun 3 4) ""]]
                    ?showVar

testSharedString7 :: TestTree
testSharedString7 =
    testSubscription @(StringUpdate String) "SharedString7" "ABCD" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \main -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 3)) mainSub
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \_sect -> do
                    ?showVar
                    subDoEdits main [[StringReplaceSection (startEndRun 2 4) "PQR"]]
                    ?showVar

testSharedString7a :: TestTree
testSharedString7a =
    testSubscription @(StringUpdate String) "SharedString7a" "AB" $ \mainSub ->
        subscribeEditor mainSub $
        testOutputEditor "main" $ \main -> do
            lensSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
            subscribeEditor lensSub $
                testOutputEditor "sect" $ \_sect -> do
                    ?showVar
                    subDoEdits main [[StringReplaceSection (startEndRun 2 2) "PQR"]]
                    ?showVar

testSubscribe :: TestTree
testSubscribe =
    testGroup
        "subscribe"
        [ testUpdateObject
        , testPair
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
        , testSharedString7a
        ]
