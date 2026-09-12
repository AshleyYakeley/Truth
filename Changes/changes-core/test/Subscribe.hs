{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    )
where

import GHC.Conc qualified as Conc
import Shapes
import Shapes.Test
import System.Timeout qualified as Timeout

import Changes.Core
import Test.Useful

orderedLines :: MonadIO m => (?outputLn :: String -> IO ()) => ((?outputLn :: String -> IO ()) => m a) -> m a
orderedLines ma = do
    buffer <- liftIO $ newMVar []
    a <- let
        ?outputLn = \s -> modifyMVar_ buffer $ \r -> return $ r <> [s]
        in ma
    liftIO $ do
        ?outputLn "ordered {"
        unorderedLines <- takeMVar buffer
        for_ (sort unorderedLines) ?outputLn
        ?outputLn "ordered }"
    return a

debugLens ::
    forall updateA updateB.
    (Show (UpdateEdit updateA), Show (UpdateEdit updateB), ?outputLn :: String -> IO ()) =>
    String ->
    ChangeLens updateA updateB ->
    ChangeLens updateA updateB
debugLens name (MkChangeLens g u pe) = let
    u' ::
        forall m.
        MonadIO m =>
        updateA ->
        Readable m (UpdateReader updateA) ->
        m [updateB]
    u' ua mr = do
        -- these are asynchronous, so commented out
        -- liftIO $ ?outputLn $ name ++ ": +update: " ++ show ua
        ubs <- u ua mr
        -- liftIO $ ?outputLn $ name ++ ": -update: " ++ show ubs
        return ubs
    pe' ::
        forall m.
        MonadIO m =>
        [UpdateEdit updateB] ->
        Readable m (UpdateReader updateA) ->
        m (Maybe [UpdateEdit updateA])
    pe' ebs mr = do
        liftIO $ ?outputLn $ name ++ ": +put: " ++ show ebs
        meas <- pe ebs mr
        liftIO $ ?outputLn $ name ++ ": -put: " ++ show meas
        return meas
    in MkChangeLens g u' pe'

debugFloatingLens ::
    forall updateA updateB.
    (Show (UpdateEdit updateA), Show (UpdateEdit updateB), ?outputLn :: String -> IO ()) =>
    String ->
    FloatingChangeLens updateA updateB ->
    FloatingChangeLens updateA updateB
debugFloatingLens name = floatLift (\mr -> mr) $ debugLens name

doModelTest :: TestName -> ((?outputLn :: String -> IO ()) => View ()) -> TestTree
doModelTest name call = goldenTest "." name $ runLifecycle $ let
    ?outputLn = hPutStrLn ?handle
    in runView call

testUpdateFunction ::
    forall a.
    (?outputLn :: String -> IO (), Show a) =>
    IO () ->
    ChangeLens (WholeUpdate a) (ROWUpdate a)
testUpdateFunction signal = let
    clRead :: ReadFunction (WholeReader a) (WholeReader a)
    clRead mr = mr
    clUpdate ::
        forall m.
        MonadIO m =>
        WholeUpdate a ->
        Readable m (WholeReader a) ->
        m [ROWUpdate a]
    clUpdate (MkWholeReaderUpdate s) mr = do
        s' <- mr ReadWhole
        liftIO $ ?outputLn $ "lens update edit: " <> show s
        liftIO $ ?outputLn $ "lens update MR: " <> show s'
        liftIO signal
        return [MkReadOnlyUpdate $ MkWholeReaderUpdate s]
    in MkChangeLens{clPutEdits = clPutEditsNone, ..}

barrier :: IO (IO (), IO ())
barrier = do
    mvar <- newEmptyMVar
    let
        signal = putMVar mvar ()
        wait = takeMVar mvar
    return (signal, wait)

testSharedModelUpdatesTask :: TestTree
testSharedModelUpdatesTask =
    testTree "shared model waits for downstream update work" $ runLifecycle $ do
        model <- makeMemoryModel ()
        callbackStarted <- liftIO newEmptyMVar
        allowDelivery <- liftIO newEmptyMVar
        events <- liftIO newEmptyMVar
        pendingTask <- liftIO $ newMVar mempty
        (finishDownstream, downstreamTask) <- liftIO $ mkTask @IO @()
        let
            trackedDownstreamTask =
                downstreamTask
                    { taskWait = do
                        putMVar events "downstream wait"
                        taskWait downstreamTask
                    }
            receive _ _ _ = do
                putMVar callbackStarted ()
                readMVar allowDelivery
                void $ swapMVar pendingTask trackedDownstreamTask
            -- With delivery held at the barrier, the model waiter can only
            -- block on the subscription runner. This avoids a scheduling sleep.
            waitUntilBlocked tid = do
                status <- Conc.threadStatus tid
                case status of
                    Conc.ThreadBlocked Conc.BlockedOnMVar -> return ()
                    Conc.ThreadRunning -> Conc.yield >> waitUntilBlocked tid
                    _ -> assertFailure $ "unexpected model waiter status: " <> show status
        tunnel $ \tun -> runResource emptyResourceContext model $ \amodel ->
            tun $ aModelSubscribe amodel (ioTask $ readMVar pendingTask) receive
        lifecycleOnClose $ do
            void $ tryPutMVar allowDelivery ()
            finishDownstream ()
        liftIO $ do
            runResource emptyResourceContext model $ \amodel ->
                pushOrFail "failed" noEditSource $ aModelEdit amodel $ pure $ MkWholeReaderEdit ()
            readMVar callbackStarted
            (waiter, tid) <- forkTask $ do
                taskWait $ modelUpdatesTask model
                putMVar events "model wait returned"
            waitUntilBlocked tid
            -- The callback now installs work that did not exist when waiting
            -- began. Waiting must discover it after the runner finishes.
            putMVar allowDelivery ()
            firstEvent <- takeMVar events
            finishDownstream ()
            taskWait waiter
            assertEqual "modelUpdatesTask returned before waiting for downstream work" "downstream wait" firstEvent

testDynamicModel :: TestTree
testDynamicModel =
    testTree "dynamic model switches subscriptions" $ runLifecycle $ do
        firstModel <- makeMemoryModel (1 :: Int)
        secondModel <- makeMemoryModel (10 :: Int)
        outerModel <- makeMemoryModel firstModel
        let model = dynamicModel $ modelToReadOnly outerModel
        received <- liftIO $ newMVar []
        let
            receive _ updates _ = modifyMVar_ received $ \values ->
                return $ values <> fmap (\(MkWholeReaderUpdate value) -> value) (toList updates)
            push m value = runResource emptyResourceContext m $ \am ->
                pushOrFail "edit failed" noEditSource $ aModelEdit am $ pure $ MkWholeReaderEdit value
            checkResult expected = do
                taskWait $ modelUpdatesTask model
                values <- swapMVar received []
                assertEqual "updates" expected values
        tunnel $ \tun -> runResource emptyResourceContext model $ \am ->
            tun $ aModelSubscribe am mempty receive
        liftIO $ do
            push firstModel 2
            checkResult [2]
            push outerModel secondModel
            checkResult [10]
            push firstModel 3
            taskWait $ modelUpdatesTask firstModel
            checkResult []
            push secondModel 11
            checkResult [11]
            push model 12
            checkResult [12]
            push outerModel firstModel
            checkResult [3]
            push firstModel 4
            checkResult [4]

testDynamicModelCallbackResources :: TestTree
testDynamicModelCallbackResources =
    testTree "dynamic model releases resources before notifying" $ runLifecycle $ do
        firstModel <- makeMemoryModel (1 :: Int)
        secondModel <- makeMemoryModel (10 :: Int)
        outerModel <- makeMemoryModel firstModel
        let model = dynamicModel $ modelToReadOnly outerModel
        received <- liftIO newEmptyMVar
        let
            readValue m = runResource emptyResourceContext m $ \am -> aModelRead am ReadWhole
            receive _ _ _ = do
                -- A GUI callback waits for another thread, which may already
                -- be handling input and need these same model resources.
                value <- Timeout.timeout 1000000 $ pusherWait forkIOPusher $ do
                    innerModel <- readValue outerModel
                    readValue innerModel
                putMVar received value
        tunnel $ \tun -> runResource emptyResourceContext model $ \am ->
            tun $ aModelSubscribe am mempty receive
        liftIO $ do
            runResource emptyResourceContext outerModel $ \am ->
                pushOrFail "edit failed" noEditSource $ aModelEdit am $ pure $ MkWholeReaderEdit secondModel
            taskWait $ modelUpdatesTask model
            takeMVar received >>= assertEqual "callback can access source and inner resources" (Just 10)

testRunEachHere :: TestTree
testRunEachHere = testTree "runEachHere retains its model" $ do
    started <- newMVar (0 :: Int)
    closed <- newMVar (0 :: Int)
    let
        increment var = modifyMVar_ var $ return . succ
        checkCounts starts closes = do
            readMVar started >>= assertEqual "actions run" starts
            readMVar closed >>= assertEqual "lifecycles closed" closes
    runLifecycle $ do
        firstModel <- makeMemoryModel (1 :: Int)
        secondModel <- makeMemoryModel (10 :: Int)
        let
            action model = do
                liftIO $ increment started
                lifecycleOnClose $ increment closed
                return model
        source <- makeMemoryModel $ action firstModel
        -- Initialization must reuse the source resource already held here.
        result <- tunnel $ \tun -> runResourceContext emptyResourceContext source $ \rc _ _ ->
            tun $ wModelRunEachHere rc $ MkWModel $ modelToReadOnly source
        let
            model = dynamicModel $ unWModel result
            readValue = runResource emptyResourceContext model $ \am -> aModelRead am ReadWhole
            push m value = runResource emptyResourceContext m $ \am ->
                pushOrFail "edit failed" noEditSource $ aModelEdit am $ pure $ MkWholeReaderEdit value
        liftIO $ do
            readValue >>= assertEqual "first read" 1
            readValue >>= assertEqual "second read" 1
            checkCounts 1 0
        received <- for [1 :: Int, 2] $ \_ -> do
            values <- liftIO $ newMVar []
            tunnel $ \tun -> runResource emptyResourceContext model $ \am ->
                tun $ aModelSubscribe am mempty $ \_ updates _ ->
                    modifyMVar_ values $ \old ->
                        return $ old <> fmap (\(MkWholeUpdate value) -> value) (toList updates)
            return values
        let
            checkResult expected = do
                taskWait $ modelUpdatesTask model
                for_ received $ \values -> swapMVar values [] >>= assertEqual "updates" expected
        liftIO $ do
            push firstModel 2
            checkResult [2]
            readValue >>= assertEqual "updated read" 2
            checkCounts 1 0
            push source $ action secondModel
            checkResult [10]
            readValue >>= assertEqual "replacement read" 10
            checkCounts 2 1
            push firstModel 3
            taskWait $ modelUpdatesTask firstModel
            checkResult []
            push secondModel 11
            checkResult [11]
            checkCounts 2 1
    checkCounts 2 2

testUpdateReference :: TestTree
testUpdateReference =
    repeatTest 100
        $ doModelTest "updateReference"
        $ do
            obj <- liftIO $ makeMemoryReference "old" $ \_ -> True
            var <- liftIO $ newEmptyMVar
            (signal, wait) <- liftIO $ barrier
            let
                om :: Premodel (WholeUpdate String) ()
                om = reflectingPremodel obj
                lens :: FloatingChangeLens (WholeUpdate String) (WholeUpdate String)
                lens = changeLensToFloating $ fromReadOnlyRejectingChangeLens . testUpdateFunction signal
                recv :: String -> IO () -> ResourceContext -> NonEmpty (WholeUpdate String) -> EditContext -> IO ()
                recv name w _ ee _ = do
                    randomSleep
                    putMVar var $ do
                        randomSleep
                        w
                        for_ ee $ \(MkWholeReaderUpdate s) -> ?outputLn $ name <> " update edit: " <> show s
                showAction :: IO ()
                showAction = do
                    randomSleep
                    action <- takeMVar var
                    action
            rc <- viewGetResourceContext
            omr' <-
                viewLiftLifecycle $ do
                    randomSleep
                    om' <- sharePremodel om
                    omr' <- om' rc mempty $ recv "recv" wait
                    _ <- mapPremodel rc lens (om' rc) mempty $ recv "recv" (return ())
                    return omr'
            viewRunResource (pmrReference omr') $ \MkAReference{..} ->
                pushOrFail "failed" noEditSource $ refEdit $ pure $ MkWholeReaderEdit "new"
            liftIO showAction
            liftIO showAction
            liftIO $ taskWait $ pmrUpdatesTask omr'

outputLn :: (?outputLn :: String -> IO (), MonadIO m) => String -> m ()
outputLn s = liftIO $ ?outputLn s

outputNameLn :: (?outputLn :: String -> IO (), MonadIO m) => String -> String -> m ()
outputNameLn name s = outputLn $ name ++ ": " ++ s

subscribeShowUpdatesF :: Show update => String -> Model update -> View ((String -> IO ()) -> View ())
subscribeShowUpdatesF name model = do
    chan <- liftIO newChan
    viewOnCloseIO $ do
        threadDelay 1000 -- 1ms to allow for updates to finish
        writeChan chan Nothing
        final <- readChan chan
        -- verify that update has been shown
        case final of
            Nothing -> return ()
            Just update -> fail $ name <> ": update left over: " <> show update
    viewBindModel model Nothing (return ()) mempty $ \() updates ->
        for_ updates $ \update -> liftIO $ writeChan chan $ Just update
    return
        $ \ol ->
            let
                ?outputLn = ol
                in liftIO $ do
                    mupdate <- readChan chan
                    case mupdate of
                        Just update -> outputNameLn name $ "receive " ++ show update
                        Nothing -> fail "premature end of updates"

subscribeShowUpdates :: (Show update, ?outputLn :: String -> IO ()) => String -> Model update -> View (View ())
subscribeShowUpdates name model = do
    su <- subscribeShowUpdatesF name model
    return $ su ?outputLn

showModelSubject ::
    (Show (UpdateSubject update), FullSubjectReader (UpdateReader update), ?outputLn :: String -> IO ()) =>
    String ->
    Model update ->
    View ()
showModelSubject name model = do
    liftIO $ taskWait $ modelUpdatesTask model
    viewRunResource model $ \asub -> do
        val <- readableToSubject $ aModelRead asub
        outputNameLn name $ "get " ++ show val

modelPushEdits ::
    (Show (UpdateEdit update), ?outputLn :: String -> IO ()) =>
    String ->
    Model update ->
    [NonEmpty (UpdateEdit update)] ->
    View ()
modelPushEdits name model editss =
    viewRunResource model $ \asub ->
        for_ editss $ \edits -> do
            outputNameLn name $ "push " ++ show (toList edits)
            maction <- aModelEdit asub edits
            case maction of
                Nothing -> outputNameLn name "push disallowed"
                Just action -> do
                    action noEditSource
                    outputNameLn name $ "push succeeded"

modelDontPushEdits ::
    (Show (UpdateEdit update), ?outputLn :: String -> IO ()) =>
    String ->
    Model update ->
    [NonEmpty (UpdateEdit update)] ->
    View ()
modelDontPushEdits name model editss =
    viewRunResource model $ \asub ->
        for_ editss $ \edits -> do
            outputNameLn name $ "push " ++ show (toList edits)
            maction <- aModelEdit asub edits
            case maction of
                Nothing -> outputNameLn name "push disallowed"
                Just _action -> outputNameLn name "push ignored"

testSubscription ::
    forall update.
    (?outputLn :: String -> IO (), IsUpdate update, FullEdit (UpdateEdit update), Show (UpdateSubject update)) =>
    UpdateSubject update ->
    View (Model update, View (), NonEmpty (UpdateEdit update) -> View ())
testSubscription initial = do
    iow <- liftIO $ newIOWitness
    var <- liftIO $ newMVar initial
    let
        varObj :: Reference (WholeEdit (UpdateSubject update))
        varObj = mvarReference iow var $ \_ -> True
        editObj :: Reference (UpdateEdit update)
        editObj = convertReference varObj
    model <- viewLiftLifecycle $ makeReflectingModel editObj
    let
        showVar = liftIO $ withMVar var $ \s -> ?outputLn $ "var: " ++ show s
        showExpected =
            \edits ->
                liftIO
                    $ withMVar var
                    $ \s -> do
                        news <- readableToSubject $ applyEdits (toList edits) $ subjectToReadable s
                        ?outputLn $ "expected: " ++ show news
    return (model, showVar, showExpected)

testPair :: TestTree
testPair =
    doModelTest "Pair" $ do
        (mainModel, mainShow, mainShowExpected) <-
            testSubscription @(PairUpdate (WholeUpdate Bool) (WholeUpdate Bool)) (False, False)
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        mainShow
        mainShowExpected
            $ (MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True)
            :| [MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True]
        modelPushEdits
            "main"
            mainModel
            [ (MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True)
                :| [MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True]
            ]
        mainShowUpdate
        mainShowUpdate
        mainShow

testString :: TestTree
testString =
    doModelTest "String" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        mainShow
        modelDontPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 3 5) "PQR"]
        mainShow
        modelDontPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 3) ""]
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 1 2) "xy"]
        mainShowUpdate
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShowUpdate
        mainShow

testString1 :: TestTree
testString1 =
    doModelTest "String1" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        mainShow
        modelDontPushEdits
            "main"
            mainModel
            [pure $ StringReplaceSection (startEndRun 3 5) "PQR", pure $ StringReplaceSection (startEndRun 2 3) ""]
        mainShow
        modelPushEdits
            "main"
            mainModel
            [pure $ StringReplaceSection (startEndRun 1 2) "xy", pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShowUpdate
        mainShowUpdate
        mainShow

testString2 :: TestTree
testString2 =
    doModelTest "String2" $ do
        (mainModel, mainShow, mainShowExpected) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        mainShow
        modelDontPushEdits
            "main"
            mainModel
            [(StringReplaceSection (startEndRun 3 5) "PQR") :| [StringReplaceSection (startEndRun 2 3) ""]]
        mainShow
        mainShowExpected $ (StringReplaceSection (startEndRun 1 2) "xy") :| [StringReplaceSection (startEndRun 2 4) "1"]
        modelPushEdits
            "main"
            mainModel
            [(StringReplaceSection (startEndRun 1 2) "xy") :| [StringReplaceSection (startEndRun 2 4) "1"]]
        mainShowUpdate
        mainShowUpdate
        mainShow

testSharedString1 :: TestTree
testSharedString1 =
    doModelTest "SharedString1" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 4)) mainModel
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelDontPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 3 5) "PQR"]
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 1 2) "xy"]
        mainShowUpdate
        sectShowUpdate
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString2 :: TestTree
testSharedString2 =
    doModelTest "SharedString2" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainModel
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelPushEdits "sect" sectModel [pure $ StringReplaceSection (startEndRun 0 0) "P"]
        mainShowUpdate
        sectShowUpdate
        mainShow
        modelPushEdits "sect" sectModel [pure $ StringReplaceSection (startEndRun 0 0) "Q"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString3 :: TestTree
testSharedString3 =
    doModelTest "SharedString3" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainModel
        runEditor sectModel $ pure ()
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 1 1) "P"]
        mainShowUpdate
        sectShowUpdate
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 2) "Q"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString4 :: TestTree
testSharedString4 =
    doModelTest "SharedString4" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainModel
        runEditor sectModel $ pure ()
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 0 0) "P"]
        mainShowUpdate
        showModelSubject "sect" sectModel
        mainShow
        modelPushEdits "sect" sectModel [pure $ StringReplaceSection (startEndRun 0 0) "Q"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString5 :: TestTree
testSharedString5 =
    doModelTest "SharedString5" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 3)) mainModel
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 4) ""]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString6 :: TestTree
testSharedString6 =
    doModelTest "SharedString6" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 3)) mainModel
        showModelSubject "sect" sectModel
        _sectFlush <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 3 4) ""]
        mainShowUpdate
        mainShow

testSharedString7 :: TestTree
testSharedString7 =
    doModelTest "SharedString7" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 3)) mainModel
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 4) "PQR"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString7a :: TestTree
testSharedString7a =
    doModelTest "SharedString7a" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "AB"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainModel
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 2) "PQR"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testPairedStrings1 :: TestTree
testPairedStrings1 =
    doModelTest "PairedStrings1" $ do
        (sub1, _, _) <- testSubscription @(StringUpdate String) "ABC"
        (sub2, _, _) <- testSubscription @(StringUpdate String) "PQR"
        let pairSub = pairModels sub1 sub2
        pairShowUpdate <- subscribeShowUpdates "pair" pairSub
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        pairShowUpdate

testPairedStrings2 :: TestTree
testPairedStrings2 =
    doModelTest "PairedStrings2" $ do
        (sub1, showVar1, _) <- testSubscription @(StringUpdate String) "ABC"
        (sub2, showVar2, _) <- testSubscription @(StringUpdate String) "PQR"
        showModelSubject "sub1" sub1
        sub1ShowUpdate <- subscribeShowUpdates "sub1" sub1
        showModelSubject "sub2" sub2
        sub2ShowUpdate <- subscribeShowUpdates "sub2" sub2
        let pairSub = pairModels sub1 sub2
        showModelSubject "pair" pairSub
        pairShowUpdate <- subscribeShowUpdates "pair" pairSub
        showVar1
        showVar2
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        sub1ShowUpdate
        pairShowUpdate
        showVar1
        showVar2
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 2 2) "y"]
        sub2ShowUpdate
        pairShowUpdate
        showVar1
        showVar2

testPairedString1 :: TestTree
testPairedString1 =
    doModelTest "PairedString1" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        let pairSub = pairModels mainModel mainModel
        showModelSubject "pair" pairSub
        pairShowUpdate <- subscribeShowUpdates "pair" pairSub
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow

testPairedSharedString1 :: TestTree
testPairedSharedString1 =
    doModelTest "PairedSharedString1" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "PABCQ"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 4)) mainModel
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        let pairSub = pairModels sectModel sectModel
        showModelSubject "pair" pairSub
        pairShowUpdate <- subscribeShowUpdatesF "pair" pairSub
        let
            pairShowUpdates =
                orderedLines $ do
                    pairShowUpdate ?outputLn
                    pairShowUpdate ?outputLn
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdates
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdates
        mainShow

testPairedSharedString2 :: TestTree
testPairedSharedString2 =
    doModelTest "PairedSharedString2" $ do
        (mainModel, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainModel
        mainShowUpdate <- subscribeShowUpdates "main" mainModel
        sectModel <- viewFloatMapModel (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainModel
        showModelSubject "sect" sectModel
        sectShowUpdate <- subscribeShowUpdates "sect" sectModel
        let pairSub = pairModels sectModel sectModel
        showModelSubject "pair" pairSub
        pairShowUpdate <- subscribeShowUpdatesF "pair" pairSub
        let
            pairShowUpdates =
                orderedLines $ do
                    pairShowUpdate ?outputLn
                    pairShowUpdate ?outputLn
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 1 1) "P"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdates
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 2) "Q"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdates
        mainShow
        modelPushEdits "sect" sectModel [pure $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdates
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdates
        mainShow

testSubscribe :: TestTree
testSubscribe =
    testTree
        "subscribe"
        [ testDynamicModel
        , testDynamicModelCallbackResources
        , testRunEachHere
        , testSharedModelUpdatesTask
        , testUpdateReference
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
        , testPairedStrings1
        , testPairedStrings2
        , testPairedString1
        , testPairedSharedString1
        , testPairedSharedString2
        ]
