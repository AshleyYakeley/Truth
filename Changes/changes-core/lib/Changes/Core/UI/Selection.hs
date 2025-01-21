module Changes.Core.UI.Selection where

import Changes.Core.Import
import Changes.Core.Model
import Changes.Core.Types
import Changes.Core.UI.View.View

newtype SelectNotify sel = MkSelectNotify
    { runSelectNotify :: View (Maybe sel) -> View ()
    }

instance Semigroup (SelectNotify sel) where
    MkSelectNotify p <> MkSelectNotify q = MkSelectNotify $ \vms -> p vms >> q vms

instance Monoid (SelectNotify sel) where
    mempty = MkSelectNotify $ \_ -> return ()

instance Contravariant SelectNotify where
    contramap f (MkSelectNotify sela) = MkSelectNotify $ \vmb -> sela $ fmap (fmap f) vmb

viewLiftSelectNotify :: SelectNotify sel -> SelectNotify (View sel)
viewLiftSelectNotify (MkSelectNotify sn) =
    MkSelectNotify $ \vmvs ->
        sn $ do
            mvs <- vmvs
            for mvs id

mapMaybeSelectNotify :: (selb -> Maybe sela) -> SelectNotify sela -> SelectNotify selb
mapMaybeSelectNotify f (MkSelectNotify sela) = MkSelectNotify $ \vmb -> sela $ fmap (\mselb -> mselb >>= f) vmb

makePremodelSelectNotify :: forall sel. Premodel (ROWUpdate (Maybe sel)) (SelectNotify sel)
makePremodelSelectNotify utask recv =
    fmap (fmap $ \update -> MkSelectNotify $ \vms -> liftIOWithUnlift $ \unlift -> update $ \_ -> unlift vms)
        $ notifyingPremodel Nothing utask recv
