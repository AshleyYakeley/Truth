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
    runResource rc sub $ \asub ->
        aModelSubscribe asub mempty $ \_ updates _ec -> do
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
            uo :: UpdateOrder (ContextUpdate UpdateX (ConstWholeUpdate Char))
            uo =
                mkUpdateOrder (compare @Int)
                    $ funcChangeLens
                    $ \(MkWithContext lm c) ->
                        case lookupItem c lm of
                            Just (_, i) -> i
                            Nothing -> 0
            flens ::
                FloatingChangeLens (ContextUpdate UpdateX (FiniteSetUpdate Char)) (ContextUpdate UpdateX (OrderedListUpdate (ConstWholeUpdate Char)))
            flens = contextOrderedSetLens uo
        rawContextObj :: Reference (WholeEdit [(Char, Int)]) <-
            makeMemoryReference [('A', 10), ('B', 20), ('C', 30), ('D', 40), ('E', 50)] $ \_ -> True
        rawContentObj :: Reference (WholeEdit (ListSet Char)) <- makeMemoryReference (setFromList "ABCDE") $ \_ -> True
        let
            contextObj :: Reference (UpdateEdit UpdateX)
            contextObj = mapReference (convertChangeLens @(WholeUpdate [(Char, Int)]) @UpdateX) rawContextObj
            baseContentObj :: Reference (FiniteSetEdit Char)
            baseContentObj =
                mapReference (convertChangeLens @(WholeUpdate (ListSet Char)) @(FiniteSetUpdate Char)) rawContentObj
        getUpdates <-
            runLifecycle $ do
                contextSub <- makeReflectingModel @UpdateX contextObj
                baseContentSub <- makeReflectingModel @(FiniteSetUpdate Char) baseContentObj
                let
                    bothSub :: Model (ContextUpdate UpdateX (FiniteSetUpdate Char))
                    bothSub = contextModels contextSub baseContentSub
                olSub <- floatMapModel rc flens bothSub
                getUpdates <- collectModelUpdates rc $ mapModel (tupleChangeLens SelectContent) olSub
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
testLens = testTree "lens" [testContextOrderedSetLens]
