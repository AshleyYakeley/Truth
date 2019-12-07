{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.ExpectedFailure
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
        in do
               hSetBuffering ?handle NoBuffering
               call

goldenTest' :: TestName -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest' name call = goldenTest name ("test/golden/" ++ name ++ ".ref") ("test/golden/" ++ name ++ ".out") call

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
            recv :: NonEmpty (WholeUpdate String) -> EditContext -> IO ()
            recv ee _ = for_ ee $ \(MkWholeReaderUpdate s) -> hPutStrLn ?handle $ "recv update edit: " <> show s
            recv' :: NonEmpty (WholeUpdate String) -> EditContext -> IO ()
            recv' ee _ = for_ ee $ \(MkWholeReaderUpdate s) -> hPutStrLn ?handle $ "recv' update edit: " <> show s
        runLifeCycle $ do
            om' <- shareObjectMaker om
            (MkResource trun MkAnObject {..}, ()) <- om' recv
            (_obj', ()) <- mapObjectMaker lens om' recv'
            runResourceRunnerWith trun $ \run ->
                liftIO $ run $ do pushOrFail "failed" noEditSource $ objEdit $ pure $ MkWholeReaderEdit "new"
            return ()

outputLn :: (?handle :: Handle, MonadIO m) => String -> m ()
outputLn s = liftIO $ hPutStrLn ?handle s

outputNameLn :: (?handle :: Handle, MonadIO m) => String -> String -> m ()
outputNameLn name s = outputLn $ name ++ ": " ++ s

subscribeShowUpdates' ::
       ( Show update
       , Show (UpdateEdit update)
       , Show (UpdateSubject update)
       , FullSubjectReader (UpdateReader update)
       , ?handle :: Handle
       )
    => String
    -> Subscriber update
    -> LifeCycleIO ()
subscribeShowUpdates' name (MkResource trun (MkASubscriber _ subrecv)) =
    runResourceRunnerWith trun $ \run ->
        remonad run $ subrecv $ \edits _ -> outputNameLn name $ "receive " ++ show edits

subscribeShowUpdates ::
       ( Show update
       , Show (UpdateEdit update)
       , Show (UpdateSubject update)
       , FullSubjectReader (UpdateReader update)
       , ?handle :: Handle
       )
    => String
    -> Subscriber update
    -> LifeCycleIO ()
subscribeShowUpdates name (MkResource trun (MkASubscriber (MkAnObject mr _) subrecv)) =
    runResourceRunnerWith trun $ \run ->
        remonad run $
        subrecv $ \edits _ -> do
            outputNameLn name $ "receive " ++ show edits
            val <- run $ mutableReadToSubject mr
            outputNameLn name $ "receive " ++ show val

showSubscriberSubject ::
       (Show (UpdateSubject update), FullSubjectReader (UpdateReader update), ?handle :: Handle)
    => String
    -> Subscriber update
    -> LifeCycleIO ()
showSubscriberSubject name (MkResource trun (MkASubscriber (MkAnObject mr _) _)) =
    liftIO $
    runResourceRunnerWith trun $ \run ->
        run $ do
            val <- mutableReadToSubject mr
            outputNameLn name $ "get " ++ show val

subscriberPushEdits ::
       (Show (UpdateEdit update), ?handle :: Handle)
    => String
    -> Subscriber update
    -> [NonEmpty (UpdateEdit update)]
    -> LifeCycleIO ()
subscriberPushEdits name (MkResource trun (MkASubscriber (MkAnObject _ push) _)) editss =
    runResourceRunnerWith trun $ \run ->
        liftIO $
        run $
        for_ editss $ \edits -> do
            outputNameLn name $ "push " ++ show edits
            maction <- push edits
            case maction of
                Nothing -> outputNameLn name "push disallowed"
                Just action -> do
                    action noEditSource
                    outputNameLn name $ "push succeeded"

subscriberDontPushEdits ::
       (Show (UpdateEdit update), ?handle :: Handle)
    => String
    -> Subscriber update
    -> [NonEmpty (UpdateEdit update)]
    -> LifeCycleIO ()
subscriberDontPushEdits name (MkResource trun (MkASubscriber (MkAnObject _ push) _)) editss =
    runResourceRunnerWith trun $ \run ->
        liftIO $
        run $
        for_ editss $ \edits -> do
            outputNameLn name $ "push " ++ show edits
            maction <- push edits
            case maction of
                Nothing -> outputNameLn name "push disallowed"
                Just _action -> outputNameLn name "push ignored"

testSubscription ::
       forall update. (?handle :: Handle, IsUpdate update, FullEdit (UpdateEdit update), Show (UpdateSubject update))
    => UpdateSubject update
    -> LifeCycleIO (Subscriber update, LifeCycleIO (), NonEmpty (UpdateEdit update) -> LifeCycleIO ())
testSubscription initial = do
    iow <- liftIO $ newIOWitness
    var <- liftIO $ newMVar initial
    let
        varObj :: Object (WholeEdit (UpdateSubject update))
        varObj = mvarObject iow var $ \_ -> True
        editObj :: Object (UpdateEdit update)
        editObj = convertObject varObj
    sub <- makeReflectingSubscriber SynchronousUpdateTiming editObj
    let
        showVar = liftIO $ withMVar var $ \s -> hPutStrLn ?handle $ "var: " ++ show s
        showExpected =
            \edits ->
                liftIO $
                withMVar var $ \s -> do
                    news <- mutableReadToSubject $ applyEdits (toList edits) $ subjectToMutableRead s
                    hPutStrLn ?handle $ "expected: " ++ show news
    return (sub, showVar, showExpected)

doSubscriberTest :: TestName -> ((?handle :: Handle) => LifeCycleIO ()) -> TestTree
doSubscriberTest name call = goldenTest' name $ runLifeCycle $ call

testPair :: TestTree
testPair =
    doSubscriberTest "Pair" $ do
        (mainSub, mainShow, mainShowExpected) <-
            testSubscription @(PairUpdate (WholeUpdate Bool) (WholeUpdate Bool)) (False, False)
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        mainShow
        mainShowExpected $
            (MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True) :|
            [MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True]
        subscriberPushEdits
            "main"
            mainSub
            [ (MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True) :|
              [MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True]
            ]
        mainShow

testString :: TestTree
testString =
    doSubscriberTest "String" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        mainShow
        subscriberDontPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 3 5) "PQR"]
        mainShow
        subscriberDontPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 3) ""]
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 2) "xy"]
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShow

testString1 :: TestTree
testString1 =
    doSubscriberTest "String1" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        mainShow
        subscriberDontPushEdits
            "main"
            mainSub
            [pure $ StringReplaceSection (startEndRun 3 5) "PQR", pure $ StringReplaceSection (startEndRun 2 3) ""]
        mainShow
        subscriberPushEdits
            "main"
            mainSub
            [pure $ StringReplaceSection (startEndRun 1 2) "xy", pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShow

testString2 :: TestTree
testString2 =
    doSubscriberTest "String2" $ do
        (mainSub, mainShow, mainShowExpected) <- testSubscription @(StringUpdate String) "ABCDE"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        mainShow
        subscriberDontPushEdits
            "main"
            mainSub
            [(StringReplaceSection (startEndRun 3 5) "PQR") :| [StringReplaceSection (startEndRun 2 3) ""]]
        mainShow
        mainShowExpected $ (StringReplaceSection (startEndRun 1 2) "xy") :| [StringReplaceSection (startEndRun 2 4) "1"]
        subscriberPushEdits
            "main"
            mainSub
            [(StringReplaceSection (startEndRun 1 2) "xy") :| [StringReplaceSection (startEndRun 2 4) "1"]]
        mainShow

testSharedString1 :: TestTree
testSharedString1 =
    doSubscriberTest "SharedString1" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 4)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberDontPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 3 5) "PQR"]
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 2) "xy"]
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShow

testSharedString2 :: TestTree
testSharedString2 =
    doSubscriberTest "SharedString2" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 0 0) "P"]
        mainShow
        subscriberPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 0 0) "Q"]
        mainShow

testSharedString3 :: TestTree
testSharedString3 =
    doSubscriberTest "SharedString3" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
        subscribeEditor sectSub $ pure ()
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 1) "P"]
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 2) "Q"]
        mainShow

testSharedString4 :: TestTree
testSharedString4 =
    doSubscriberTest "SharedString4" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
        subscribeEditor sectSub $ pure ()
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 0 0) "P"]
        showSubscriberSubject "sect" sectSub
        mainShow
        subscriberPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 0 0) "Q"]
        mainShow

testSharedString5 :: TestTree
testSharedString5 =
    doSubscriberTest "SharedString5" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 3)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) ""]
        mainShow

testSharedString6 :: TestTree
testSharedString6 =
    doSubscriberTest "SharedString6" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 3)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 3 4) ""]
        mainShow

testSharedString7 :: TestTree
testSharedString7 =
    doSubscriberTest "SharedString7" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 3)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) "PQR"]
        mainShow

testSharedString7a :: TestTree
testSharedString7a =
    doSubscriberTest "SharedString7a" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "AB"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 2) "PQR"]
        mainShow

testPairedStrings1 :: TestTree
testPairedStrings1 =
    doSubscriberTest "PairedStrings1" $ do
        (sub1, showVar1, _) <- testSubscription @(StringUpdate String) "ABC"
        (sub2, showVar2, _) <- testSubscription @(StringUpdate String) "PQR"
        showSubscriberSubject "sub1" sub1
        subscribeShowUpdates "sub1" sub1
        showSubscriberSubject "sub2" sub2
        subscribeShowUpdates "sub2" sub2
        let pairSub = pairSubscribers sub1 sub2
        showSubscriberSubject "pair" pairSub
        subscribeShowUpdates' "pair" pairSub
        showVar1
        showVar2
        subscriberPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        showVar1
        showVar2
        subscriberPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 2 2) "y"]
        showVar1
        showVar2

testPairedString1 :: TestTree
testPairedString1 =
    doSubscriberTest "PairedString1" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        let pairSub = pairSubscribers mainSub mainSub
        showSubscriberSubject "pair" pairSub
        subscribeShowUpdates "pair" pairSub
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        mainShow
        subscriberPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShow
        subscriberPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShow

testPairedSharedString1 :: TestTree
testPairedSharedString1 =
    doSubscriberTest "PairedSharedString1" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "PABCQ"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 4)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        let pairSub = pairSubscribers sectSub sectSub
        showSubscriberSubject "pair" pairSub
        subscribeShowUpdates "pair" pairSub
        mainShow
        subscriberPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShow
        subscriberPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShow

testPairedSharedString2 :: TestTree
testPairedSharedString2 =
    doSubscriberTest "PairedSharedString2" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showSubscriberSubject "main" mainSub
        subscribeShowUpdates "main" mainSub
        sectSub <- mapSubscriber (fmap (debugLens "lens") $ stringSectionLens (startEndRun 1 2)) mainSub
        showSubscriberSubject "sect" sectSub
        subscribeShowUpdates "sect" sectSub
        let pairSub = pairSubscribers sectSub sectSub
        showSubscriberSubject "pair" pairSub
        subscribeShowUpdates "pair" pairSub
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 1) "P"]
        mainShow
        subscriberPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 2) "Q"]
        mainShow
        subscriberPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShow
        subscriberPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShow

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
        , ignoreTest testPairedStrings1
        , ignoreTest testPairedString1
        , ignoreTest testPairedSharedString1
        , ignoreTest testPairedSharedString2
        ]
