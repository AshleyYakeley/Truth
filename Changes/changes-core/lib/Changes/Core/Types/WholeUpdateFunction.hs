module Changes.Core.Types.WholeUpdateFunction where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Whole

newtype WholeUpdateFunction update a = MkWholeUpdateFunction
    { unWholeUpdateFunction :: ChangeLens update (ROWUpdate a)
    }

instance Functor (WholeUpdateFunction update) where
    fmap :: forall a b. (a -> b) -> WholeUpdateFunction update a -> WholeUpdateFunction update b
    fmap ab (MkWholeUpdateFunction (MkChangeLens g u _)) = let
        g' :: ReadFunction (UpdateReader update) (WholeReader b)
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
        g' :: ReadFunction (UpdateReader update) (WholeReader a)
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

instance IsoVariant (WholeUpdateFunction update) where
    isoMap ab _ = fmap ab
