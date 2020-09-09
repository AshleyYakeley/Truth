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
    { unWholeUpdateFunction :: ChangeLens update ((ROWUpdate a))
    }

instance Functor (WholeUpdateFunction update) where
    fmap :: forall a b. (a -> b) -> WholeUpdateFunction update a -> WholeUpdateFunction update b
    fmap ab (MkWholeUpdateFunction (MkChangeLens g u _)) = let
        g' :: forall m t. MonadIO m
           => Readable m (UpdateReader update)
           -> WholeReader b t
           -> m t
        g' mr ReadWhole = fmap ab $ g mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> Readable m (UpdateReader update)
           -> m [ROWUpdate b]
        u' update mr =
            fmap (fmap $ \(MkReadOnlyUpdate (MkWholeReaderUpdate a)) -> MkReadOnlyUpdate $ MkWholeReaderUpdate $ ab a) $
            u update mr
        in MkWholeUpdateFunction $ MkChangeLens g' u' clPutEditsNone

instance Applicative (WholeUpdateFunction update) where
    pure :: forall a. a -> WholeUpdateFunction update a
    pure a = let
        g' :: forall m t. MonadIO m
           => Readable m (UpdateReader update)
           -> WholeReader a t
           -> m t
        g' _mr ReadWhole = return a
        u' :: forall m. MonadIO m
           => update
           -> Readable m (UpdateReader update)
           -> m [ROWUpdate a]
        u' _update _mr = return []
        in MkWholeUpdateFunction $ MkChangeLens g' u' clPutEditsNone
    MkWholeUpdateFunction p <*> MkWholeUpdateFunction q =
        MkWholeUpdateFunction $
        liftReadOnlyChangeLens (funcChangeLens $ \(ab, a) -> ab a) .
        readOnlyPairChangeLens . pairCombineChangeLenses p q

instance IsoVariant (WholeUpdateFunction update)
