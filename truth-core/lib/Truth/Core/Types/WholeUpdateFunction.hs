module Truth.Core.Types.WholeUpdateFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Tuple.Pair
import Truth.Core.Types.Whole

newtype WholeUpdateFunction update a = MkWholeUpdateFunction
    { unWholeUpdateFunction :: EditLens update (ReadOnlyUpdate (WholeUpdate a))
    }

instance Functor (WholeUpdateFunction update) where
    fmap :: forall a b. (a -> b) -> WholeUpdateFunction update a -> WholeUpdateFunction update b
    fmap ab (MkWholeUpdateFunction (MkEditLens g u _)) = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader b t
           -> m t
        g' mr ReadWhole = fmap ab $ g mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> m [ReadOnlyUpdate (WholeUpdate b)]
        u' update mr =
            fmap (fmap $ \(MkReadOnlyUpdate (MkWholeReaderUpdate a)) -> MkReadOnlyUpdate $ MkWholeReaderUpdate $ ab a) $
            u update mr
        in MkWholeUpdateFunction $ MkEditLens g' u' elPutEditsNone

instance Applicative (WholeUpdateFunction update) where
    pure :: forall a. a -> WholeUpdateFunction update a
    pure a = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader a t
           -> m t
        g' _mr ReadWhole = return a
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> m [ReadOnlyUpdate (WholeUpdate a)]
        u' _update _mr = return []
        in MkWholeUpdateFunction $ MkEditLens g' u' elPutEditsNone
    MkWholeUpdateFunction p <*> MkWholeUpdateFunction q =
        MkWholeUpdateFunction $
        liftReadOnlyEditLens (funcEditLens $ \(ab, a) -> ab a) . readOnlyPairEditLens . pairCombineEditLenses p q

instance IsoVariant (WholeUpdateFunction update)
