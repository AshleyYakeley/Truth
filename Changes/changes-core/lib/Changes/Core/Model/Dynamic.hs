module Changes.Core.Model.Dynamic (dynamicModel, dynamicWModel) where

import Changes.Core.Import
import Changes.Core.Model.Model
import Changes.Core.Model.Reference
import Changes.Core.Model.WModel
import Changes.Core.Resource
import Changes.Core.Types

liftReader :: ReaderT () IO a -> ReaderT x IO a
liftReader r = lift $ runReaderT r ()

dynamicAReference :: AReference update (AReference update ())
dynamicAReference =
    MkAReference
        { refRead = \rt -> do
            aref <- ask
            liftReader $ refRead aref rt
        , refEdit = \edits -> do
            aref <- ask
            maction <- liftReader $ refEdit aref edits
            return $ fmap (fmap liftReader) maction
        , refCommitTask = pure () -- wrong
        }

dynamicAModel :: AModel update (AModel update ())
dynamicAModel =
    MkAModel
        { aModelAReference = contramap aModelAReference dynamicAReference
        , aModelSubscribe = \task update -> do
            amodel <- lift ask
            hoist liftReader (aModelSubscribe amodel task update)
        , aModelUpdatesTask = pure () -- wrong
        }

mapResourceRunner :: (a -> b) -> ResourceRunner a -> ResourceRunner b
mapResourceRunner _ _ = error "NYI"

dynamicModel :: forall update. Model (ROWUpdate (Model update)) -> Model update
dynamicModel (MkResource runner1 am1) =
    MkResource
        ( dependentResourceRunner runner1
            $ \t -> do
                MkResource runner2 am2 <- runReaderT (aModelRead am1 ReadWhole) t
                return $ mapResourceRunner (\t2 -> contramap (\() -> t2) am2) runner2
        )
        dynamicAModel

dynamicWModel :: forall update. WModel (ROWUpdate (WModel update)) -> WModel update
dynamicWModel wmodel = MkWModel $ dynamicModel $ unWModel $ eaMapReadOnlyWhole unWModel wmodel
