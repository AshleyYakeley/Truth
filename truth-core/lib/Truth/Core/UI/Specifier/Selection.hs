module Truth.Core.UI.Specifier.Selection where

import Data.IORef
import Truth.Core.Import
import Truth.Core.UI.View.View

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

makeRefSelectNotify :: forall sel. IO (SelectNotify sel, View (Maybe sel))
makeRefSelectNotify = do
    ref :: IORef (View (Maybe sel)) <- newIORef $ return Nothing
    let
        setsel vms = liftIO $ writeIORef ref vms
        getsel = do
            vmsel <- liftIO $ readIORef ref
            vmsel
    return (MkSelectNotify setsel, getsel)
