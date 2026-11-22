module Lens
    ( testLens
    )
where

import Shapes
import Shapes.Test

import Changes.Core

collectModelUpdates :: ResourceContext -> Model update -> Lifecycle (IO [update])
collectModelUpdates rc sub = do
    var <- liftIO $ newMVar []
    tunnel $ \tun -> runResource rc sub $ \asub ->
        tun $ aModelSubscribe asub mempty $ \_ updates _ec -> do
            mVarRunStateT var $ do
                uu <- get
                put $ uu <> toList updates
    return $ takeMVar var

modelPushEdits :: ResourceContext -> Model update -> NonEmpty (UpdateEdit update) -> IO ()
modelPushEdits rc sub edits = do
    runResource rc sub $ \asub -> do
        mpush <- aModelEdit asub edits
        case mpush of
            Nothing -> fail "can't push edits"
            Just push -> push noEditSource
    taskWait $ modelUpdatesTask sub

type UpdateX = KeyUpdate [(Char, Int)] (PairUpdate (ConstWholeUpdate Char) (WholeUpdate Int))

updToString :: OrderedListUpdate (ConstWholeUpdate Char) -> String
updToString (OrderedListUpdateItem a b []) = "move " <> show a <> " -> " <> show b
updToString (OrderedListUpdateItem _ _ (update : _)) = never update
updToString (OrderedListUpdateDelete a) = "del " <> show a
updToString (OrderedListUpdateInsert a c) = "ins " <> show a <> " " <> show c
updToString OrderedListUpdateClear = "clear"

-- arrange a list of characters in an order
-- the order is given in the context as a map from Char to Int
testContextOrderedSetLensCase :: [(Char, Int)] -> [(SequencePoint, SequencePoint)] -> TestTree
testContextOrderedSetLensCase assigns expected =
    testTree (show assigns) $ do
        let
            rc :: ResourceContext
            rc = emptyResourceContext
            uo :: UpdateOrder (ContextUpdate UpdateX (KeyValueUpdate Char (NoUpdate ())))
            uo =
                mkUpdateOrder (compare @Int)
                    $ funcChangeLens
                    $ \(MkWithContext lm (c, ())) ->
                        case lookupItem c lm of
                            Just (_, i) -> i
                            Nothing -> 0
            flens ::
                FloatingChangeLens (ContextUpdate UpdateX (FiniteSetUpdate Char)) (ContextUpdate UpdateX (OrderedListUpdate (KeyValueUpdate Char (NoUpdate ()))))
            flens = contextOrderedSetLens uo
        rawContextObj :: Reference (WholeEdit [(Char, Int)]) <-
            makeMemoryReference [('A', 10), ('B', 20), ('C', 30), ('D', 40), ('E', 50)] $ \_ -> True
        rawContentObj :: Reference (WholeEdit (ListMap Char ())) <- makeMemoryReference (listSetToMap (const ()) $ setFromList "ABCDE") $ \_ -> True
        let
            contextObj :: Reference (UpdateEdit UpdateX)
            contextObj = mapReference (convertChangeLens @(WholeUpdate [(Char, Int)]) @UpdateX) rawContextObj
            baseContentObj :: Reference (FiniteSetEdit Char)
            baseContentObj =
                mapReference (convertChangeLens @(WholeUpdate (ListMap Char ())) @(FiniteSetUpdate Char)) rawContentObj
        getUpdates <-
            runLifecycle $ do
                contextSub <- makeReflectingModel @UpdateX contextObj
                baseContentSub <- makeReflectingModel @(FiniteSetUpdate Char) baseContentObj
                let
                    bothSub :: Model (ContextUpdate UpdateX (FiniteSetUpdate Char))
                    bothSub = contextModels contextSub baseContentSub
                olSub <- floatMapModel rc flens bothSub
                getUpdates <- collectModelUpdates rc $ mapModel (liftOrderedListChangeLens (tupleChangeLens SelectFirst) . tupleChangeLens SelectContent) olSub
                let
                    pushOneEdit :: (Char, Int) -> Lifecycle ()
                    pushOneEdit (c, i) =
                        liftIO
                            $ modelPushEdits rc contextSub
                            $ pure
                            $ KeyEditItem c
                            $ MkTupleUpdateEdit SelectSecond
                            $ MkWholeReaderEdit i
                for_ assigns pushOneEdit
                return getUpdates
        let
            expectedUpdates :: [OrderedListUpdate (ConstWholeUpdate Char)]
            expectedUpdates = fmap (\(a, b) -> OrderedListUpdateItem a b []) expected
        foundUpdates <- getUpdates
        assertEqual "updates" (fmap updToString expectedUpdates) (fmap updToString foundUpdates)

testContextOrderedSetLens :: TestTree
testContextOrderedSetLens =
    testTree
        "contextOrderedSetLens"
        [ testContextOrderedSetLensCase [('A', 25)] [(0, 1)]
        , testContextOrderedSetLensCase [('A', 25), ('B', 20)] [(0, 1)]
        , testContextOrderedSetLensCase [('A', 25), ('A', 10)] [(0, 1), (1, 0)]
        , testContextOrderedSetLensCase [('A', 25), ('A', 11)] [(0, 1), (1, 0)]
        , testContextOrderedSetLensCase [('A', 25), ('B', 27)] [(0, 1), (0, 1)]
        , testContextOrderedSetLensCase [('B', 17)] []
        , testContextOrderedSetLensCase [('B', 5)] [(1, 0)]
        ]

testLens :: TestTree
testLens = testTree "lens" [testContextOrderedSetLens, testAlignMapLens, testMapMaybeMapLens, testFiniteSetLatticeLens, testMapKeysLenses, testMapCartesianSumLens, testFilterMapLens, testCollectMapKeysLens]

testAlignMapLens :: TestTree
testAlignMapLens = testTree "alignMapChangeLens" $ do
    let
        lens = alignMapChangeLens @Char @(WholeUpdate Int) @(WholeUpdate Int)
        source :: Readable IO (PairUpdateReader (MapUpdate Char (WholeUpdate Int)) (MapUpdate Char (WholeUpdate Int)))
        source =
            pairReadable
                (wholeMapReadFunction $ subjectToReadable [('a', 1 :: Int), ('c', 3)])
                (wholeMapReadFunction $ subjectToReadable [('b', 2 :: Int), ('c', 4)])
        readAt :: Readable IO (PairUpdateReader (MapUpdate Char (WholeUpdate Int)) (MapUpdate Char (WholeUpdate Int))) -> Char -> IO (Maybe (These Int Int))
        readAt mr k = clRead lens mr $ MapReadValue k ReadWhole
    for_ [('a', Just $ This 1), ('b', Just $ That 2), ('c', Just $ These 3 4), ('d', Nothing)] $ \(k, expected) -> do
        found <- readAt source k
        assertEqual "read" expected found
    for_ [This 5, That 6, These 7 8] $ \v -> do
        medits <- clPutEdits lens [MapEditSet 'c' v] source
        case medits of
            Nothing -> fail "write rejected"
            Just edits -> do
                found <- readAt (applyEdits edits source) 'c'
                assertEqual "write" (Just v) found
    medits <- clPutEdits lens [MapEditClear, MapEditValue 'c' $ MkWholeReaderEdit $ This 9] source
    case medits of
        Nothing -> fail "clear rejected"
        Just edits -> do
            found <- clRead lens (applyEdits edits source) MapReadKeys
            assertEqual "clear then edit absent key" [] $ setToList found
    let
        cleared :: Readable IO (PairUpdateReader (MapUpdate Char (WholeUpdate Int)) (MapUpdate Char (WholeUpdate Int)))
        cleared = applyEdit (MkTupleUpdateEdit SelectFirst MapEditClear) source
    updates <- clUpdate lens (MkTupleUpdate SelectFirst MapUpdateClear) cleared
    case updates of
        [MapUpdateClear, MapUpdateSet 'b' (That 2), MapUpdateSet 'c' (That 4)] -> return ()
        _ -> fail "clearing the left map must preserve right entries"

-- Compare incremental updates with fresh reads after each source edit.
checkReadOnlyMapLens ::
    forall key source value.
    (IsUpdate source, ApplicableEdit (UpdateEdit source), Ord key, Show key, Eq value, Show value) =>
    ChangeLens source (ReadOnlyUpdate (MapUpdate key (WholeUpdate value))) ->
    Readable IO (UpdateReader source) ->
    [UpdateEdit source] ->
    IO ()
checkReadOnlyMapLens lens initial edits = go initial (clRead lens initial) edits
    where
        snapshot :: Readable IO (MapReader key (WholeUpdate value)) -> IO [(key, Maybe value)]
        snapshot mr = do
            ks <- mr MapReadKeys
            for (sort $ setToList ks) $ \k -> do
                v <- mr $ MapReadValue k ReadWhole
                return (k, v)
        go :: Readable IO (UpdateReader source) -> Readable IO (MapReader key (WholeUpdate value)) -> [UpdateEdit source] -> IO ()
        go _ _ [] = return ()
        go source target (edit : rest) = do
            let
                source' :: Readable IO (UpdateReader source)
                source' = applyEdit edit source
            updates <- clUpdate lens (editUpdate edit) source'
            let
                target' :: Readable IO (MapReader key (WholeUpdate value))
                target' = applyEdits (fmap (updateEdit . unReadOnlyUpdate) updates) target
            expected <- snapshot $ clRead lens source'
            actual <- snapshot target'
            assertEqual "incremental result" expected actual
            go source' target' rest

testMapMaybeMapLens :: TestTree
testMapMaybeMapLens = testTree "mapMaybeMapChangeLens" $ do
    let
        lens = mapMaybeMapChangeLens @Char @Int @Int $ \a -> if a > 0 then Just (a * 10) else Nothing
        source :: Readable IO (MapReader Char (WholeUpdate Int))
        source = wholeMapReadFunction $ subjectToReadable [('a', 1 :: Int), ('b', -1)]
    ks <- clRead lens source MapReadKeys
    assertEqual "filtered keys" "a" $ setToList ks
    for_ [('a', Just 'a', Just 10), ('b', Nothing, Nothing), ('z', Nothing, Nothing)] $ \(k, key, value) -> do
        foundKey <- clRead lens source $ MapReadKey k
        foundValue <- clRead lens source $ MapReadValue k ReadWhole
        assertEqual "key" key foundKey
        assertEqual "value" value foundValue
    updates <- clUpdate lens (MapUpdateSet 'a' (-2)) source
    case updates of
        [MkReadOnlyUpdate (MapUpdateDelete 'a')] -> return ()
        _ -> fail "rejected replacement must delete only its key"
    checkReadOnlyMapLens
        lens
        source
        [ MapEditValue 'b' $ MkWholeReaderEdit 2
        , MapEditValue 'a' $ MkWholeReaderEdit (-1)
        , MapEditValue 'b' $ MkWholeReaderEdit 3
        , MapEditSet 'b' (-2)
        , MapEditSet 'c' 4
        , MapEditDelete 'c'
        , MapEditClear
        , MapEditSet 'd' 5
        ]

testFiniteSetLatticeLens :: TestTree
testFiniteSetLatticeLens = testTree "finite set lattice"
    $ for_ [("union", joinChangeLens, "abc"), ("intersection", meetChangeLens, "b")]
    $ \(name, lens, expected) -> do
        let
            source :: Readable IO (PairUpdateReader (FiniteSetUpdate Char) (FiniteSetUpdate Char))
            source =
                pairReadable
                    (wholeMapReadFunction $ subjectToReadable [('a', ()), ('b', ())])
                    (wholeMapReadFunction $ subjectToReadable [('b', ()), ('c', ())])
            observed :: ChangeLens (PairUpdate (FiniteSetUpdate Char) (FiniteSetUpdate Char)) (ReadOnlyUpdate (MapUpdate Char (WholeUpdate ())))
            observed =
                liftReadOnlyChangeLens
                    (toReadOnlyChangeLens . liftMapElementChangeLens (\() -> pure Nothing) (convertChangeLens @(NoUpdate ()) @(WholeUpdate ())))
                    . lens
        ks <- clRead lens source MapReadKeys
        assertEqual name expected $ sort $ setToList ks
        checkReadOnlyMapLens
            observed
            source
            [ MkTupleUpdateEdit SelectFirst $ MapEditSet 'c' ()
            , MkTupleUpdateEdit SelectSecond $ MapEditDelete 'b'
            , MkTupleUpdateEdit SelectFirst MapEditClear
            , MkTupleUpdateEdit SelectFirst $ MapEditSet 'c' ()
            , MkTupleUpdateEdit SelectSecond MapEditClear
            , MkTupleUpdateEdit SelectSecond $ MapEditSet 'd' ()
            ]

-- Exercise values as well as membership: unit-valued sets would miss lost
-- value reads, edits, and updates in key-only transformations.
testMapKeysLenses :: TestTree
testMapKeysLenses = testTree "map key lenses" $ do
    let
        source :: Readable IO (MapReader Char (WholeUpdate Int))
        source = wholeMapReadFunction $ subjectToReadable [('a', 1 :: Int), ('z', 9)]
        filtered = filterMapKeysChangeLens @Char @(WholeUpdate Int) (/= 'z')
        permissive = codecMapKeysChangeLens @Char @Char @(WholeUpdate Int) $ ifCodec (/= 'z')
        renamed = bijectionMapKeysChangeLens @Char @Char @(WholeUpdate Int) $ MkIsomorphism succ pred
        snapshot :: Readable IO (MapReader Char (WholeUpdate Int)) -> IO [(Char, Maybe Int)]
        snapshot mr = do
            ks <- mr MapReadKeys
            for (sort $ setToList ks) $ \k -> fmap (k,) $ mr $ MapReadValue k ReadWhole
        putAndRead ::
            ChangeLens (MapUpdate Char (WholeUpdate Int)) (MapUpdate Char (WholeUpdate Int)) ->
            [MapEdit Char (WholeUpdate Int)] ->
            IO [(Char, Maybe Int)]
        putAndRead lens edits = do
            me <- clPutEdits lens edits source
            case me of
                Nothing -> fail "write unexpectedly rejected"
                Just es -> snapshot $ applyEdits es source
    for_ [filtered, permissive] $ \lens -> do
        v <- clRead lens source $ MapReadValue 'z' ReadWhole
        assertEqual "hidden value" Nothing v
        present <- clRead lens source $ MapReadValue 'a' ReadWhole
        assertEqual "visible value" (Just 1) present
        checkReadOnlyMapLens
            (toReadOnlyChangeLens . lens)
            source
            [ MapEditValue 'a' $ MkWholeReaderEdit 2
            , MapEditValue 'z' $ MkWholeReaderEdit 10
            , MapEditSet 'b' 3
            , MapEditDelete 'a'
            , MapEditClear
            ]
        result <- putAndRead lens [MapEditSet 'b' 2, MapEditClear, MapEditSet 'c' 3, MapEditClear]
        assertEqual "batched clears preserve only hidden entries" [('z', Just 9)] result
        result' <- putAndRead lens [MapEditValue 'a' $ MkWholeReaderEdit 7]
        assertEqual "value edits pass through" [('a', Just 7), ('z', Just 9)] result'
    rejected <- clPutEdits filtered [MapEditSet 'z' 0] source
    assertEqual "hidden insertion rejected" True $ isNothing rejected
    rejectedEdit <- clPutEdits filtered [MapEditValue 'z' $ MkWholeReaderEdit 0] source
    assertEqual "hidden value edit rejected" True $ isNothing rejectedEdit
    ignored <- putAndRead filtered [MapEditDelete 'z']
    assertEqual "hidden deletion ignored" [('a', Just 1), ('z', Just 9)] ignored
    hiddenWrite <- putAndRead permissive [MapEditSet 'z' 8]
    assertEqual "codec permits hidden writes" [('a', Just 1), ('z', Just 8)] hiddenWrite
    hiddenDelete <- putAndRead permissive [MapEditDelete 'z']
    assertEqual "codec permits hidden deletes" [('a', Just 1)] hiddenDelete
    rv <- clRead renamed source $ MapReadValue 'b' ReadWhole
    assertEqual "renamed value" (Just 1) rv
    renamedWrite <- putAndRead renamed [MapEditValue 'b' $ MkWholeReaderEdit 4, MapEditSet 'c' 5]
    assertEqual "renamed writes" [('a', Just 4), ('b', Just 5), ('z', Just 9)] renamedWrite
    checkReadOnlyMapLens
        (toReadOnlyChangeLens . renamed)
        source
        [MapEditValue 'a' $ MkWholeReaderEdit 2, MapEditSet 'b' 3, MapEditDelete 'z', MapEditClear]

testMapCartesianSumLens :: TestTree
testMapCartesianSumLens = testTree "mapCartesianSumChangeLens" $ do
    let
        lens = mapCartesianSumChangeLens @Char @Char @(WholeUpdate Int)
        source :: Readable IO (PairUpdateReader (MapUpdate Char (WholeUpdate Int)) (MapUpdate Char (WholeUpdate Int)))
        source =
            pairReadable
                (wholeMapReadFunction $ subjectToReadable [('a', 1 :: Int)])
                (wholeMapReadFunction $ subjectToReadable [('a', 2 :: Int), ('b', 3)])
    for_ [(Left 'a', Just 1), (Right 'a', Just 2), (Left 'b', Nothing)] $ \(k, expected) -> do
        actual <- clRead lens source $ MapReadValue k ReadWhole
        assertEqual "sum value" expected actual
    checkReadOnlyMapLens
        (toReadOnlyChangeLens . lens)
        source
        [ MkTupleUpdateEdit SelectFirst $ MapEditValue 'a' $ MkWholeReaderEdit 4
        , MkTupleUpdateEdit SelectSecond $ MapEditValue 'a' $ MkWholeReaderEdit 5
        , MkTupleUpdateEdit SelectFirst MapEditClear
        , MkTupleUpdateEdit SelectFirst $ MapEditSet 'c' 6
        , MkTupleUpdateEdit SelectSecond MapEditClear
        , MkTupleUpdateEdit SelectSecond $ MapEditSet 'd' 7
        , MkTupleUpdateEdit SelectFirst $ MapEditDelete 'c'
        ]
    me <-
        clPutEdits
            lens
            [MapEditValue (Left 'a') $ MkWholeReaderEdit 8, MapEditSet (Right 'c') 9, MapEditDelete (Right 'a')]
            source
    case me of
        Nothing -> fail "sum writes rejected"
        Just es -> do
            let
                mr :: Readable IO (PairUpdateReader (MapUpdate Char (WholeUpdate Int)) (MapUpdate Char (WholeUpdate Int)))
                mr = applyEdits es source
            leftValue <- firstReadFunction mr $ MapReadValue 'a' ReadWhole
            rightValue <- secondReadFunction mr $ MapReadValue 'c' ReadWhole
            deleted <- secondReadFunction mr $ MapReadKey 'a'
            assertEqual "left edit" (Just 8) leftValue
            assertEqual "right insert" (Just 9) rightValue
            assertEqual "right delete" Nothing deleted
    cleared <- clPutEdits lens [MapEditClear] source
    case cleared of
        Nothing -> fail "sum clear rejected"
        Just es -> do
            ks <- clRead lens (applyEdits es source) MapReadKeys
            assertEqual "both sides cleared" [] $ setToList ks

    let
        unitLens = finiteSetCartesianSumChangeLens @Char @Char
        unitSource :: Readable IO (PairUpdateReader (FiniteSetUpdate Char) (FiniteSetUpdate Char))
        unitSource =
            pairReadable
                (wholeMapReadFunction $ subjectToReadable [('a', ())])
                (wholeMapReadFunction $ subjectToReadable [('b', ())])
        afterClear :: Readable IO (PairUpdateReader (FiniteSetUpdate Char) (FiniteSetUpdate Char))
        afterClear = applyEdit (MkTupleUpdateEdit SelectFirst MapEditClear) unitSource
    unitUpdates <- clUpdate unitLens (MkTupleUpdate SelectFirst MapUpdateClear) afterClear
    let
        unitResult :: Readable IO (FiniteSetReader (Either Char Char))
        unitResult = applyEdits (fmap updateEdit unitUpdates) $ clRead unitLens unitSource
    unitKeys <- unitResult MapReadKeys
    assertEqual "finite-set sum retains the other side" [Right 'b'] $ setToList unitKeys

testFilterMapLens :: TestTree
testFilterMapLens = testTree "filterMapUpdateFunction" $ do
    let
        lens = filterMapUpdateFunction @Char @(WholeUpdate Int)
        source :: Readable IO (PairUpdateReader (MapUpdate Char (WholeUpdate Int)) (PartialSetUpdate Char))
        source =
            pairReadable
                (wholeMapReadFunction $ subjectToReadable [('a', 1 :: Int), ('b', 2)])
                (subjectToReadable $ \k -> k == 'a')
        predicateEdit :: Char -> Bool -> PairUpdateEdit (MapUpdate Char (WholeUpdate Int)) (PartialSetUpdate Char)
        predicateEdit k visible = MkTupleUpdateEdit SelectSecond $ MkTupleUpdateEdit (MkFunctionSelector k) $ MkWholeReaderEdit visible
    checkReadOnlyMapLens
        lens
        source
        [ MkTupleUpdateEdit SelectFirst $ MapEditValue 'b' $ MkWholeReaderEdit 3
        , predicateEdit 'b' True
        , predicateEdit 'a' False
        , predicateEdit 'z' True
        , MkTupleUpdateEdit SelectFirst $ MapEditSet 'z' 4
        , MkTupleUpdateEdit SelectFirst $ MapEditValue 'b' $ MkWholeReaderEdit 5
        , MkTupleUpdateEdit SelectFirst $ MapEditDelete 'b'
        , MkTupleUpdateEdit SelectFirst MapEditClear
        ]
    let
        changed :: Readable IO (PairUpdateReader (MapUpdate Char (WholeUpdate Int)) (PartialSetUpdate Char))
        changed = pairReadable (firstReadFunction source) (subjectToReadable $ \k -> k == 'b')
    updates <- clUpdate lens (MkTupleUpdate SelectSecond $ UnknownPartialUpdate $ \_ -> True) changed
    let
        incremental :: Readable IO (MapReader Char (WholeUpdate Int))
        incremental = applyEdits (fmap (updateEdit . unReadOnlyUpdate) updates) $ clRead lens source
    ks <- incremental MapReadKeys
    value <- incremental $ MapReadValue 'b' ReadWhole
    hidden <- incremental $ MapReadValue 'a' ReadWhole
    assertEqual "unknown predicate filters keys" "b" $ setToList ks
    assertEqual "newly visible value" (Just 2) value
    assertEqual "newly hidden value" Nothing hidden
    -- NoReader () must not manufacture an entry for an absent key.
    let
        unitSource :: Readable IO (PairUpdateReader (FiniteSetUpdate Char) (PartialSetUpdate Char))
        unitSource =
            pairReadable
                (wholeMapReadFunction $ subjectToReadable [('a', ()), ('b', ())])
                (subjectToReadable $ \k -> k == 'b')
        unitLens = filterFiniteSetUpdateFunction @Char
    missing <- clUpdate unitLens (MkTupleUpdate SelectSecond $ KnownPartialUpdate $ MkTupleUpdate (MkFunctionSelector 'z') $ MkWholeReaderUpdate True) unitSource
    case missing of
        [MkReadOnlyUpdate (MapUpdateDelete 'z')] -> return ()
        _ -> fail "absent unit-valued entry was inserted"
    rebuilt <- clUpdate unitLens (MkTupleUpdate SelectSecond $ UnknownPartialUpdate $ \_ -> True) unitSource
    case rebuilt of
        [MkReadOnlyUpdate MapUpdateClear, MkReadOnlyUpdate (MapUpdateSet 'b' ())] -> return ()
        _ -> fail "finite-set rebuild must filter keys"

testCollectMapKeysLens :: TestTree
testCollectMapKeysLens = testTree "collectMapKeysChangeLens" $ do
    let
        lens = collectMapKeysChangeLens @Char @Char @(WholeUpdate Int) id
        source :: Readable IO (PairUpdateReader (ROWUpdate (Char -> Maybe Char)) (MapUpdate Char (WholeUpdate Int)))
        source =
            pairReadable
                (subjectToReadable $ \k -> if k == 'a' then Just k else Nothing)
                (wholeMapReadFunction $ subjectToReadable [('a', 1 :: Int), ('b', 2)])
        nextCodec k = if k == 'b' then Just k else Nothing
        changed :: Readable IO (PairUpdateReader (ROWUpdate (Char -> Maybe Char)) (MapUpdate Char (WholeUpdate Int)))
        changed = pairReadable (subjectToReadable nextCodec) (secondReadFunction source)
    updates <- clUpdate lens (MkTupleUpdate SelectFirst $ MkReadOnlyUpdate $ MkWholeReaderUpdate nextCodec) changed
    let
        incremental :: Readable IO (MapReader Char (WholeUpdate Int))
        incremental = applyEdits (fmap updateEdit updates) $ clRead lens source
    ks <- incremental MapReadKeys
    value <- incremental $ MapReadValue 'b' ReadWhole
    assertEqual "changing codec keys" "b" $ setToList ks
    assertEqual "changing codec preserves values" (Just 2) value
