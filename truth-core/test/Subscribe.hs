{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    ) where

import Shapes
import Test.Tasty
import Test.Useful
import Truth.Core

debugLens ::
       forall updateA updateB.
       (Show updateA, Show updateB, Show (UpdateEdit updateA), Show (UpdateEdit updateB), ?handle :: Handle)
    => String
    -> ChangeLens updateA updateB
    -> ChangeLens updateA updateB
debugLens name (MkChangeLens g u pe) = let
    u' :: forall m. MonadIO m
       => updateA
       -> Readable m (UpdateReader updateA)
       -> m [updateB]
    u' ua mr = do
        -- these are asynchronous, so commented out
        --liftIO $ hPutStrLn ?handle $ name ++ ": +update: " ++ show ua
        ubs <- u ua mr
        --liftIO $ hPutStrLn ?handle $ name ++ ": -update: " ++ show ubs
        return ubs
    pe' :: forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    pe' ebs mr = do
        liftIO $ hPutStrLn ?handle $ name ++ ": +put: " ++ show ebs
        meas <- pe ebs mr
        liftIO $ hPutStrLn ?handle $ name ++ ": -put: " ++ show meas
        return meas
    in MkChangeLens g u' pe'

debugFloatingLens ::
       forall updateA updateB.
       (Show updateA, Show updateB, Show (UpdateEdit updateA), Show (UpdateEdit updateB), ?handle :: Handle)
    => String
    -> FloatingChangeLens updateA updateB
    -> FloatingChangeLens updateA updateB
debugFloatingLens name = floatLift (\mr -> mr) $ debugLens name

goldenTest' :: TestName -> ((?handle :: Handle, ?rc :: ResourceContext) => IO ()) -> TestTree
goldenTest' name call =
    goldenTest "." name $ let
        ?rc = emptyResourceContext
        in call

testUpdateFunction ::
       forall a. (?handle :: Handle, Show a)
    => ChangeLens (WholeUpdate a) (ROWUpdate a)
testUpdateFunction = let
    clRead :: ReadFunction (WholeReader a) (WholeReader a)
    clRead mr = mr
    clUpdate ::
           forall m. MonadIO m
        => WholeUpdate a
        -> Readable m (WholeReader a)
        -> m [ROWUpdate a]
    clUpdate (MkWholeReaderUpdate s) mr = do
        s' <- mr ReadWhole
        liftIO $ hPutStrLn ?handle $ "lens update edit: " <> show s
        liftIO $ hPutStrLn ?handle $ "lens update MR: " <> show s'
        return [MkReadOnlyUpdate $ MkWholeReaderUpdate s]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

testUpdateObject :: TestTree
testUpdateObject =
    goldenTest' "updateObject" $ do
        obj <- makeMemoryObject "old" $ \_ -> True
        var <- newEmptyMVar
        let
            om :: ObjectMaker (WholeUpdate String) ()
            om = reflectingObjectMaker obj
            lens :: FloatingChangeLens (WholeUpdate String) (WholeUpdate String)
            lens = changeLensToFloating $ fromReadOnlyRejectingChangeLens . testUpdateFunction
            recv :: ResourceContext -> NonEmpty (WholeUpdate String) -> EditContext -> IO ()
            recv _ ee _ =
                putMVar var $ for_ ee $ \(MkWholeReaderUpdate s) -> hPutStrLn ?handle $ "recv update edit: " <> show s
            recv' :: ResourceContext -> NonEmpty (WholeUpdate String) -> EditContext -> IO ()
            recv' _ ee _ =
                putMVar var $ for_ ee $ \(MkWholeReaderUpdate s) -> hPutStrLn ?handle $ "recv' update edit: " <> show s
            showAction :: IO ()
            showAction = do
                action <- takeMVar var
                action
        runLifeCycle $ do
            om' <- shareObjectMaker om
            omr' <- om' ?rc mempty recv
            _ <- mapObjectMaker ?rc lens (om' ?rc) mempty recv'
            liftIO $
                runResource ?rc (omrObject omr') $ \MkAnObject {..} ->
                    pushOrFail "failed" noEditSource $ objEdit $ pure $ MkWholeReaderEdit "new"
            liftIO showAction
            liftIO showAction
            liftIO $ taskWait $ omrUpdatesTask omr'

outputLn :: (?handle :: Handle, MonadIO m) => String -> m ()
outputLn s = liftIO $ hPutStrLn ?handle s

outputNameLn :: (?handle :: Handle, MonadIO m) => String -> String -> m ()
outputNameLn name s = outputLn $ name ++ ": " ++ s

subscribeShowUpdates ::
       ( Show update
       , Show (UpdateEdit update)
       , Show (UpdateSubject update)
       , FullSubjectReader (UpdateReader update)
       , ?handle :: Handle
       , ?rc :: ResourceContext
       )
    => String
    -> Model update
    -> LifeCycleIO (LifeCycleIO ())
subscribeShowUpdates name sub = do
    chan <- liftIO newChan
    lifeCycleClose $ do
        writeChan chan Nothing
        final <- readChan chan
        -- verify that update has been shown
        case final of
            Nothing -> return ()
            _ -> fail "updates left over"
    runResource ?rc sub $ \asub ->
        aModelSubscribe asub mempty $ \_ updates _ -> for_ updates $ \update -> writeChan chan $ Just update
    return $ do
        mupdate <- liftIO $ readChan chan
        case mupdate of
            Just update -> outputNameLn name $ "receive " ++ show update
            Nothing -> fail "premature end of updates"

showModelSubject ::
       (Show (UpdateSubject update), FullSubjectReader (UpdateReader update), ?handle :: Handle, ?rc :: ResourceContext)
    => String
    -> Model update
    -> LifeCycleIO ()
showModelSubject name sub =
    liftIO $ do
        taskWait $ modelUpdatesTask sub
        runResource ?rc sub $ \asub -> do
            val <- readableToSubject $ aModelRead asub
            outputNameLn name $ "get " ++ show val

modelPushEdits ::
       (Show (UpdateEdit update), ?handle :: Handle, ?rc :: ResourceContext)
    => String
    -> Model update
    -> [NonEmpty (UpdateEdit update)]
    -> LifeCycleIO ()
modelPushEdits name sub editss =
    liftIO $
    runResource ?rc sub $ \asub ->
        for_ editss $ \edits -> do
            outputNameLn name $ "push " ++ show (toList edits)
            maction <- aModelEdit asub edits
            case maction of
                Nothing -> outputNameLn name "push disallowed"
                Just action -> do
                    action noEditSource
                    outputNameLn name $ "push succeeded"

modelDontPushEdits ::
       (Show (UpdateEdit update), ?handle :: Handle, ?rc :: ResourceContext)
    => String
    -> Model update
    -> [NonEmpty (UpdateEdit update)]
    -> LifeCycleIO ()
modelDontPushEdits name sub editss =
    liftIO $
    runResource ?rc sub $ \asub ->
        for_ editss $ \edits -> do
            outputNameLn name $ "push " ++ show (toList edits)
            maction <- aModelEdit asub edits
            case maction of
                Nothing -> outputNameLn name "push disallowed"
                Just _action -> outputNameLn name "push ignored"

testSubscription ::
       forall update.
       ( ?handle :: Handle
       , ?rc :: ResourceContext
       , IsUpdate update
       , FullEdit (UpdateEdit update)
       , Show (UpdateSubject update)
       )
    => UpdateSubject update
    -> LifeCycleIO (Model update, LifeCycleIO (), NonEmpty (UpdateEdit update) -> LifeCycleIO ())
testSubscription initial = do
    iow <- liftIO $ newIOWitness
    var <- liftIO $ newMVar initial
    let
        varObj :: Object (WholeEdit (UpdateSubject update))
        varObj = mvarObject iow var $ \_ -> True
        editObj :: Object (UpdateEdit update)
        editObj = convertObject varObj
    sub <- makeReflectingModel editObj
    let
        showVar = liftIO $ withMVar var $ \s -> hPutStrLn ?handle $ "var: " ++ show s
        showExpected =
            \edits ->
                liftIO $
                withMVar var $ \s -> do
                    news <- readableToSubject $ applyEdits (toList edits) $ subjectToReadable s
                    hPutStrLn ?handle $ "expected: " ++ show news
    return (sub, showVar, showExpected)

doModelTest :: TestName -> ((?handle :: Handle, ?rc :: ResourceContext) => LifeCycleIO ()) -> TestTree
doModelTest name call = goldenTest' name $ runLifeCycle call

testPair :: TestTree
testPair =
    doModelTest "Pair" $ do
        (mainSub, mainShow, mainShowExpected) <-
            testSubscription @(PairUpdate (WholeUpdate Bool) (WholeUpdate Bool)) (False, False)
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        mainShow
        mainShowExpected $
            (MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True) :|
            [MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True]
        modelPushEdits
            "main"
            mainSub
            [ (MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True) :|
              [MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True]
            ]
        mainShowUpdate
        mainShowUpdate
        mainShow

testString :: TestTree
testString =
    doModelTest "String" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        mainShow
        modelDontPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 3 5) "PQR"]
        mainShow
        modelDontPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 3) ""]
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 2) "xy"]
        mainShowUpdate
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShowUpdate
        mainShow

testString1 :: TestTree
testString1 =
    doModelTest "String1" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        mainShow
        modelDontPushEdits
            "main"
            mainSub
            [pure $ StringReplaceSection (startEndRun 3 5) "PQR", pure $ StringReplaceSection (startEndRun 2 3) ""]
        mainShow
        modelPushEdits
            "main"
            mainSub
            [pure $ StringReplaceSection (startEndRun 1 2) "xy", pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShowUpdate
        mainShowUpdate
        mainShow

testString2 :: TestTree
testString2 =
    doModelTest "String2" $ do
        (mainSub, mainShow, mainShowExpected) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        mainShow
        modelDontPushEdits
            "main"
            mainSub
            [(StringReplaceSection (startEndRun 3 5) "PQR") :| [StringReplaceSection (startEndRun 2 3) ""]]
        mainShow
        mainShowExpected $ (StringReplaceSection (startEndRun 1 2) "xy") :| [StringReplaceSection (startEndRun 2 4) "1"]
        modelPushEdits
            "main"
            mainSub
            [(StringReplaceSection (startEndRun 1 2) "xy") :| [StringReplaceSection (startEndRun 2 4) "1"]]
        mainShowUpdate
        mainShowUpdate
        mainShow

testSharedString1 :: TestTree
testSharedString1 =
    doModelTest "SharedString1" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCDE"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 4)) mainSub
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelDontPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 3 5) "PQR"]
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 2) "xy"]
        mainShowUpdate
        sectShowUpdate
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) "1"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString2 :: TestTree
testSharedString2 =
    doModelTest "SharedString2" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainSub
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 0 0) "P"]
        mainShowUpdate
        sectShowUpdate
        mainShow
        modelPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 0 0) "Q"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString3 :: TestTree
testSharedString3 =
    doModelTest "SharedString3" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainSub
        subscribeEditor ?rc sectSub $ pure ()
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 1) "P"]
        mainShowUpdate
        sectShowUpdate
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 2) "Q"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString4 :: TestTree
testSharedString4 =
    doModelTest "SharedString4" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainSub
        subscribeEditor ?rc sectSub $ pure ()
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 0 0) "P"]
        mainShowUpdate
        showModelSubject "sect" sectSub
        mainShow
        modelPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 0 0) "Q"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString5 :: TestTree
testSharedString5 =
    doModelTest "SharedString5" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 3)) mainSub
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) ""]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString6 :: TestTree
testSharedString6 =
    doModelTest "SharedString6" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 3)) mainSub
        showModelSubject "sect" sectSub
        _sectFlush <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 3 4) ""]
        mainShowUpdate
        mainShow

testSharedString7 :: TestTree
testSharedString7 =
    doModelTest "SharedString7" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABCD"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 3)) mainSub
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 4) "PQR"]
        mainShowUpdate
        sectShowUpdate
        mainShow

testSharedString7a :: TestTree
testSharedString7a =
    doModelTest "SharedString7a" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "AB"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainSub
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 2) "PQR"]
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
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        let pairSub = pairModels mainSub mainSub
        showModelSubject "pair" pairSub
        pairShowUpdate <- subscribeShowUpdates "pair" pairSub
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectFirst $ StringReplaceSection (startEndRun 1 1) "x"]
        pairShowUpdate
        mainShow
        modelPushEdits
            "pair"
            pairSub
            [pure $ MkTupleUpdateEdit SelectSecond $ StringReplaceSection (startEndRun 3 3) "y"]
        mainShowUpdate
        pairShowUpdate
        mainShow

testPairedSharedString1 :: TestTree
testPairedSharedString1 =
    doModelTest "PairedSharedString1" $ do
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "PABCQ"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 4)) mainSub
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        let pairSub = pairModels sectSub sectSub
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
        (mainSub, mainShow, _) <- testSubscription @(StringUpdate String) "ABC"
        showModelSubject "main" mainSub
        mainShowUpdate <- subscribeShowUpdates "main" mainSub
        sectSub <- floatMapModel ?rc (debugFloatingLens "lens" $ stringSectionLens (startEndRun 1 2)) mainSub
        showModelSubject "sect" sectSub
        sectShowUpdate <- subscribeShowUpdates "sect" sectSub
        let pairSub = pairModels sectSub sectSub
        showModelSubject "pair" pairSub
        pairShowUpdate <- subscribeShowUpdates "pair" pairSub
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 1 1) "P"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow
        modelPushEdits "main" mainSub [pure $ StringReplaceSection (startEndRun 2 2) "Q"]
        mainShowUpdate
        sectShowUpdate
        pairShowUpdate
        pairShowUpdate
        mainShow
        modelPushEdits "sect" sectSub [pure $ StringReplaceSection (startEndRun 1 1) "x"]
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
        , testPairedStrings1
        , testPairedStrings2
        , testPairedString1
        , testPairedSharedString1
        , testPairedSharedString2
        ]
