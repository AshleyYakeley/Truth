module List
    ( testList
    ) where

import Changes.Core
import GHC.Conc
import Shapes
import Shapes.Test

processorCountRef :: Ref IO Int
processorCountRef = MkRef getNumCapabilities setNumCapabilities

asyncTask :: Lifecycle a -> Lifecycle (Task Lifecycle a)
asyncTask pa = do
    task <- forkTask pa
    lifecycleOnClose $ do
        _ <- runLifecycle $ taskWait task
        return ()
    return task

biToMaybeWhole ::
       forall p q. Monoid p
    => ChangeLens (BiWholeUpdate p q) (BiWholeUpdate (Maybe p) (Maybe q))
biToMaybeWhole = mapBiWholeChangeLens (fromMaybe mempty) Just

actionFlushModelUpdates :: WModel update -> Lifecycle ()
actionFlushModelUpdates (MkWModel model) = liftIO $ taskWait $ modelUpdatesTask model

actionModelPush :: WModel update -> NonEmpty (UpdateEdit update) -> Lifecycle ()
actionModelPush model edits = do
    actionFlushModelUpdates model
    ok <- liftIO $ wModelPush emptyResourceContext model edits
    if ok
        then return ()
        else fail "bad push"

langListModelItem ::
       forall p q.
       Bool
    -> Int64
    -> WModel (BiUpdate (ListUpdate (WholeUpdate p)) (ListUpdate (WholeUpdate q)))
    -> Lifecycle (WModel (BiWholeUpdate (Maybe p) (Maybe q)))
langListModelItem present i lmodel = do
    let
        linearListItemCL :: forall t. LinearFloatingChangeLens _ (ListUpdate (WholeUpdate t)) (WholeUpdate (Maybe t))
        linearListItemCL =
            composeExpFloatingChangeLens (changeLensToExpFloating $ bijectionWholeChangeLens id) $
            listItemLinearLens present $ MkSequencePoint i
    eaFloatMap emptyResourceContext
            (expToFloatingChangeLens $ biLinearFloatingChangeLens (linearListItemCL @p) (linearListItemCL @q))
            lmodel

actionModelGet :: WModel update -> ReadM (UpdateReader update) t -> Lifecycle t
actionModelGet model rm = do
    actionFlushModelUpdates model
    liftIO $ wModelGet emptyResourceContext model rm

testIssue304 :: TestTree
testIssue304 = localOption (mkTimeout 3000000) $
    testTree "issue-304" $ do
        np <- getNumProcessors
        refPutRestore processorCountRef np $
            runLifecycle $
            for_ [1 .. 32] $ \(_ :: Integer) ->
                asyncTask $
                for_ [1 .. 100] $ \(_ :: Integer) -> do
                    ref <- liftIO $ makeMemoryReference mempty $ \_ -> True
                    model :: Model (ListUpdate (WholeUpdate Integer)) <- makeReflectingModel $ convertReference ref
                    let
                        lm = eaMap singleBiChangeLens $ MkWModel model
                        wm = eaMap (biToMaybeWhole . convertBiChangeLens id) lm
                    actionModelPush wm $ pure $ MkBiWholeEdit $ Just $ fromList [10, 20, 30]
                    im <- langListModelItem False 1 lm
                    actionModelPush im $ pure $ MkBiWholeEdit Nothing
                    val <- actionModelGet (eaMap biReadOnlyChangeLens wm) $ readM ReadWhole
                    let lval = fmap toList val
                    if lval == Just [10, 20, 30]
                        then return ()
                        else fail $ "bad: " <> show lval

testList :: TestTree
testList = testTree "list" [testIssue304]
