module List
    ( testList
    ) where

import Changes.Core
import GHC.Conc
import Shapes
import Shapes.Test

processorCountRef :: Ref IO Int
processorCountRef = MkRef getNumCapabilities setNumCapabilities

waitModel :: WModel update -> Lifecycle ()
waitModel model = liftIO $ taskWait $ modelUpdatesTask $ unWModel model

actionModelPush :: WModel update -> NonEmpty (UpdateEdit update) -> Lifecycle ()
actionModelPush model edits = do
    waitModel model
    ok <- liftIO $ wModelPush emptyResourceContext model edits
    if ok
        then return ()
        else fail "bad push"

langListModelItem ::
       forall t. Bool -> Int64 -> WModel (ListUpdate (WholeUpdate t)) -> Lifecycle (WModel (WholeUpdate (Maybe t)))
langListModelItem present i lmodel = do
    let
        linearListItemCL :: LinearFloatingChangeLens _ (ListUpdate (WholeUpdate t)) (WholeUpdate (Maybe t))
        linearListItemCL =
            composeExpFloatingChangeLens (changeLensToExpFloating $ bijectionWholeChangeLens id) $
            listItemLinearLens present $ MkSequencePoint i
    eaFloatMap emptyResourceContext (expToFloatingChangeLens linearListItemCL) lmodel

testIssue304 :: TestTree
testIssue304 =
    localOption (mkTimeout 3000000) $
    testTree "issue-304" $ do
        np <- getNumProcessors
        refPutRestore processorCountRef np $
            runLifecycle $
            for_ [1 .. 1000] $ \(i :: Integer) -> do
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
                im <- langListModelItem False 1 lm
                actionModelPush im $ pure $ MkWholeReaderEdit Nothing
                waitModel im
                checkRef "C" [10, 20, 30]

testList :: TestTree
testList = testTree "list" [testIssue304]
