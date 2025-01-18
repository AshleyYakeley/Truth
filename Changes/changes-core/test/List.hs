module List
    ( testList
    ) where

import Changes.Core
import GHC.Conc
import Shapes
import Shapes.Test

processorCountRef :: Ref IO Int
processorCountRef = MkRef getNumCapabilities setNumCapabilities

actionModelPush :: WModel update -> NonEmpty (UpdateEdit update) -> Lifecycle ()
actionModelPush model edits = do
    ok <- liftIO $ wModelPush emptyResourceContext model edits
    if ok
        then return ()
        else fail "bad push"

langListModelItem ::
       forall t.
       Bool
    -> SequencePoint
    -> WModel (ListUpdate (WholeUpdate t))
    -> Lifecycle (WModel (WholeUpdate (Maybe t)))
langListModelItem present i lmodel =
    eaFloatMap emptyResourceContext (changeLensToFloating (bijectionWholeChangeLens id) . listItemLens present i) lmodel

testIssue304 :: TestTree
testIssue304 =
    localOption (mkTimeout 20000000) $
    testTree "issue-304" $ do
        np <- getNumProcessors
        refPutRestore processorCountRef np $
            for_ [1 .. 5000] $ \(i :: Integer) ->
                runLifecycle $ do
                    ref <- liftIO $ makeMemoryReference mempty $ \_ -> True
                    model <- makeReflectingModel $ convertReference ref
                    let
                        checkRef :: String -> [Int] -> Lifecycle ()
                        checkRef name expected = do
                            foundv <- liftIO $ runResource emptyResourceContext ref $ \aref -> refRead aref ReadWhole
                            let found = toList foundv
                            if found == expected
                                then return ()
                                else fail $ "iter #" <> show i <> " " <> name <> ": " <> show found
                        lm :: WModel (ListUpdate (WholeUpdate Int))
                        lm = MkWModel model
                        wm = eaMap (bijectionWholeChangeLens id) lm
                    checkRef "A" []
                    actionModelPush wm $ pure $ MkWholeReaderEdit $ fromList [10, 20, 30]
                    checkRef "B" [10, 20, 30]
                    -- liftIO $ threadDelay 100 -- stops issue
                    im <- langListModelItem False 1 lm
                    checkRef "C" [10, 20, 30]
                    liftIO $ threadDelay 300
                    actionModelPush im $ pure $ MkWholeReaderEdit $ Just 15
                    checkRef "D" [10, 15, 20, 30]
                    liftIO $ taskWait $ modelCommitsTask model

testList :: TestTree
testList = testTree "list" [testIssue304]
