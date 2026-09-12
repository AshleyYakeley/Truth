module Changes.Core.Model.Dynamic (dynamicModel, dynamicWModel) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.Model
import Changes.Core.Model.Reference
import Changes.Core.Model.WModel
import Changes.Core.Resource
import Changes.Core.Types

liftReader :: ReaderT () IO a -> ReaderT x IO a
liftReader r = lift $ runReaderT r ()

dynamicAReference :: Task IO () -> AReference update (AReference update ())
dynamicAReference ctask =
    MkAReference
        { refRead = \rt -> do
            aref <- ask
            liftReader $ refRead aref rt
        , refEdit = \edits -> do
            aref <- ask
            maction <- liftReader $ refEdit aref edits
            return $ fmap (fmap liftReader) maction
        , refCommitTask = ctask
        }

dynamicAModel ::
    FullUpdate update =>
    Model (ROWUpdate (Model update)) ->
    Task IO () ->
    Task IO () ->
    AModel update (AModel (ROWUpdate (Model update)) (), AModel update ())
dynamicAModel outerModel ctask utask =
    MkAModel
        { aModelAReference = contramap (aModelAReference . snd) $ dynamicAReference ctask
        , aModelSubscribe = \task update -> do
            (outerAModel, innerAModel) <- lift ask
            (_, initialState) <- lift $ getLifeState $ hoist liftReader $ aModelSubscribe innerAModel task update
            state <- liftIO $ newMVar $ Just initialState
            lifecycleOnClose $ do
                old <- swapMVar state Nothing
                for_ old closeLifeState
            let
                receiveOuter rc _ ec = do
                    updates <- modifyMVar state $ \case
                        Nothing -> return (Nothing, [])
                        Just old -> do
                            closeLifeState old
                            runResourceContext rc outerModel $ \outerRC runOuter am -> do
                                innerModel <- runOuter $ aModelRead am ReadWhole
                                runResourceContext outerRC innerModel $ \_ run innerAM -> do
                                    (_, newState) <- getLifeState $ hoist run $ aModelSubscribe innerAM task update
                                    updates <- run $ getReplaceUpdates $ aModelRead innerAM
                                    return (Just newState, updates)
                    -- A receiver may synchronously dispatch to a GUI thread
                    -- that needs these resources to process an edit.
                    for_ (nonEmpty updates) $ \us -> update rc us ec
            hoist liftReader $ aModelSubscribe outerAModel task receiveOuter
        , aModelUpdatesTask = utask
        }

dynamicModel :: forall update. FullUpdate update => Model (ROWUpdate (Model update)) -> Model update
dynamicModel outerModel@(MkResource runner1 am1) = let
    runner :: ResourceRunner (AModel (ROWUpdate (Model update)) (), AModel update ())
    runner = dependentResourceRunner runner1
        $ \t -> do
            MkResource runner2 am2 <- runReaderT (aModelRead am1 ReadWhole) t
            return $ fmap (\t2 -> (contramap (const t) am1, contramap (const t2) am2)) runner2
    ctask :: Task IO ()
    ctask = runResourceTask runner1 $ \t -> ioTask $ do
        m2 <- runReaderT (aModelRead am1 ReadWhole) t
        pure $ modelCommitsTask m2
    utask :: Task IO ()
    utask =
        aModelUpdatesTask am1
            <> ( runResourceTask runner1 $ \t -> ioTask $ do
                    m2 <- runReaderT (aModelRead am1 ReadWhole) t
                    pure $ modelUpdatesTask m2
               )
    in MkResource runner $ dynamicAModel outerModel ctask utask

dynamicWModel :: forall update. FullUpdate update => WModel (ROWUpdate (WModel update)) -> WModel update
dynamicWModel wmodel = MkWModel $ dynamicModel $ unWModel $ eaMapReadOnlyWhole unWModel wmodel
