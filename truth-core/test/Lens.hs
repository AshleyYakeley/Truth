module Lens
    ( testLens
    ) where

import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Truth.Core

collectSubscriberUpdates :: Subscriber update -> LifeCycleIO (IO [update])
collectSubscriberUpdates (MkResource rr asub) =
    runResourceRunnerWith rr $ \run -> do
        var <- liftIO $ newMVar []
        run $
            subscribe asub $ \updates _ec ->
                mVarRun var $ do
                    uu <- get
                    put $ uu <> toList updates
        return $ takeMVar var

subscriberPushEdits :: Subscriber update -> NonEmpty (UpdateEdit update) -> IO ()
subscriberPushEdits (MkResource rr asub) edits =
    runResourceRunnerWith rr $ \run ->
        run $ do
            mpush <- subEdit asub edits
            case mpush of
                Nothing -> fail "can't push edits"
                Just push -> push noEditSource

type UpdateX = KeyUpdate [(Char, Int)] (PairUpdate (ConstWholeUpdate Char) (WholeUpdate Int))

updToString :: OrderedListUpdate String (ConstWholeUpdate Char) -> String
updToString (OrderedListUpdateItem a b Nothing) = "move " <> show a <> " -> " <> show b
updToString (OrderedListUpdateItem _ _ (Just update)) = never update
updToString (OrderedListUpdateDelete a) = "del " <> show a
updToString (OrderedListUpdateInsert a c) = "ins " <> show a <> " " <> show c
updToString OrderedListUpdateClear = "clear"

-- arrange a list of characters in an order
-- the order is given in the context as a map from Char to Int
testContextOrderedSetLens :: TestTree
testContextOrderedSetLens =
    testCase "contextOrderedSetLens" $ do
        let
            uo :: UpdateOrder (ContextUpdate UpdateX (ConstWholeUpdate Char))
            uo = MkUpdateOrder compare $ editLensToFloating $ funcEditLens $ \(MkWithContext lm c) -> lookupItem c lm
            flens ::
                   FloatingEditLens (ContextUpdate UpdateX (FiniteSetUpdate Char)) (ContextUpdate UpdateX (OrderedListUpdate String (ConstWholeUpdate Char)))
            flens = contextOrderedSetLens uo
        rawContextObj :: Object (WholeEdit [(Char, Int)]) <-
            freeIOObject [('A', 10), ('B', 20), ('C', 30), ('D', 40)] $ \_ -> True
        rawContentObj :: Object (WholeEdit (FiniteSet Char)) <- freeIOObject (setFromList "ABCD") $ \_ -> True
        let
            contextObj :: Object (UpdateEdit UpdateX)
            contextObj = mapObject (convertEditLens @(WholeUpdate [(Char, Int)]) @UpdateX) rawContextObj
            baseContentObj :: Object (FiniteSetEdit Char)
            baseContentObj =
                mapObject (convertEditLens @(WholeUpdate (FiniteSet Char)) @(FiniteSetUpdate Char)) rawContentObj
        getUpdates <-
            runLifeCycle $ do
                contextSub <- makeReflectingSubscriber @UpdateX contextObj
                baseContentSub <- makeReflectingSubscriber @(FiniteSetUpdate Char) baseContentObj
                let
                    bothSub :: Subscriber (ContextUpdate UpdateX (FiniteSetUpdate Char))
                    bothSub = contextSubscribers contextSub baseContentSub
                olSub <- floatMapSubscriber flens bothSub
                getUpdates <- collectSubscriberUpdates $ mapSubscriber (tupleEditLens SelectContent) olSub
                let
                    pushOneEdit :: Char -> Int -> LifeCycleIO ()
                    pushOneEdit c i =
                        liftIO $
                        subscriberPushEdits contextSub $
                        pure $ KeyEditItem c $ MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit i
                pushOneEdit 'A' 25
                return getUpdates
        let
            expectedUpdates :: [OrderedListUpdate String (ConstWholeUpdate Char)]
            expectedUpdates = [OrderedListUpdateItem 0 1 Nothing]
        foundUpdates <- getUpdates
        assertEqual "updates" (fmap updToString expectedUpdates) (fmap updToString foundUpdates)

testLens :: TestTree
testLens = testGroup "lens" [testContextOrderedSetLens]
