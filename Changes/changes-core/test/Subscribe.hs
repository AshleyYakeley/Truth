{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    )
where

import Shapes
import Shapes.Test

import Changes.Core
import Test.Useful

debugLens ::
    forall updateA updateB.
    (Show (UpdateEdit updateA), Show (UpdateEdit updateB), ?handle :: Handle) =>
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
        -- liftIO $ hPutStrLn ?handle $ name ++ ": +update: " ++ show ua
        ubs <- u ua mr
        -- liftIO $ hPutStrLn ?handle $ name ++ ": -update: " ++ show ubs
        return ubs
    pe' ::
        forall m.
        MonadIO m =>
        [UpdateEdit updateB] ->
        Readable m (UpdateReader updateA) ->
        m (Maybe [UpdateEdit updateA])
    pe' ebs mr = do
        liftIO $ hPutStrLn ?handle $ name ++ ": +put: " ++ show ebs
        meas <- pe ebs mr
        liftIO $ hPutStrLn ?handle $ name ++ ": -put: " ++ show meas
        return meas
    in MkChangeLens g u' pe'

debugFloatingLens ::
    forall updateA updateB.
    (Show (UpdateEdit updateA), Show (UpdateEdit updateB), ?handle :: Handle) =>
    String ->
    FloatingChangeLens updateA updateB ->
    FloatingChangeLens updateA updateB
debugFloatingLens name = floatLift (\mr -> mr) $ debugLens name

doModelTest :: TestName -> ((?handle :: Handle) => View ()) -> TestTree
doModelTest name call = goldenTest "." name $ runLifecycle $ runView call

testUpdateFunction ::
    forall a.
    (?handle :: Handle, Show a) =>
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
        liftIO $ hPutStrLn ?handle $ "lens update edit: " <> show s
        liftIO $ hPutStrLn ?handle $ "lens update MR: " <> show s'
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
                        for_ ee $ \(MkWholeReaderUpdate s) -> hPutStrLn ?handle $ name <> " update edit: " <> show s
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

outputLn :: (?handle :: Handle, MonadIO m) => String -> m ()
outputLn s = liftIO $ hPutStrLn ?handle s

outputNameLn :: (?handle :: Handle, MonadIO m) => String -> String -> m ()
outputNameLn name s = outputLn $ name ++ ": " ++ s

subscribeShowUpdates :: (Show update, ?handle :: Handle) => String -> Model update -> View (View ())
subscribeShowUpdates name model = do
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
        $ liftIO
        $ do
            mupdate <- readChan chan
            case mupdate of
                Just update -> outputNameLn name $ "receive " ++ show update
                Nothing -> fail "premature end of updates"

showModelSubject ::
    (Show (UpdateSubject update), FullSubjectReader (UpdateReader update), ?handle :: Handle) =>
    String ->
    Model update ->
    View ()
showModelSubject name model = do
    liftIO $ taskWait $ modelUpdatesTask model
    viewRunResource model $ \asub -> do
        val <- readableToSubject $ aModelRead asub
        outputNameLn name $ "get " ++ show val

modelPushEdits ::
    (Show (UpdateEdit update), ?handle :: Handle) =>
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
    (Show (UpdateEdit update), ?handle :: Handle) =>
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
    (?handle :: Handle, IsUpdate update, FullEdit (UpdateEdit update), Show (UpdateSubject update)) =>
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
        showVar = liftIO $ withMVar var $ \s -> hPutStrLn ?handle $ "var: " ++ show s
        showExpected =
            \edits ->
                liftIO
                    $ withMVar var
                    $ \s -> do
                        news <- readableToSubject $ applyEdits (toList edits) $ subjectToReadable s
                        hPutStrLn ?handle $ "expected: " ++ show news
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
        pairShowUpdate <- subscribeShowUpdates "pair" pairSub
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
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
        pairShowUpdate <- subscribeShowUpdates "pair" pairSub
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 1 1) "P"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow
        modelPushEdits "main" mainModel [pure $ StringReplaceSection (startEndRun 2 2) "Q"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow
        modelPushEdits "sect" sectModel [pure $ StringReplaceSection (startEndRun 1 1) "x"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow

testSubscribe :: TestTree
testSubscribe =
    testTree
        "subscribe"
        [ testUpdateReference
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
