module Changes.Core.Types.EditApplicative where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Whole

class EditApplicative (f :: Type -> Type) where
    eaPure ::
           forall update. SubjectReader (UpdateReader update)
        => UpdateSubject update
        -> f (ReadOnlyUpdate update)
    eaMap :: forall updateA updateB. ChangeLens updateA updateB -> f updateA -> f updateB
    eaPair :: forall updateA updateB. f updateA -> f updateB -> f (PairUpdate updateA updateB)

instance EditApplicative (ChangeLens update) where
    eaPure = constChangeLens
    eaMap = (.)
    eaPair = pairCombineChangeLenses

instance EditApplicative (FloatingChangeLens update) where
    eaPure a = changeLensToFloating $ constChangeLens a
    eaMap lens = (.) $ changeLensToFloating lens
    eaPair = pairCombineFloatingChangeLenses

eaMapSemiReadOnly ::
       forall f updateA updateB. EditApplicative f
    => ChangeLens updateA (ReadOnlyUpdate updateB)
    -> f (ReadOnlyUpdate updateA)
    -> f (ReadOnlyUpdate updateB)
eaMapSemiReadOnly lens = eaMap $ liftReadOnlyChangeLens lens

eaMapFullReadOnly ::
       forall f updateA updateB. EditApplicative f
    => ChangeLens updateA updateB
    -> f (ReadOnlyUpdate updateA)
    -> f (ReadOnlyUpdate updateB)
eaMapFullReadOnly lens = eaMapSemiReadOnly $ toReadOnlyChangeLens . lens

eaPairReadOnlyWhole :: EditApplicative f => f (ROWUpdate a) -> f (ROWUpdate b) -> f (ROWUpdate (a, b))
eaPairReadOnlyWhole fa fb =
    eaMap (liftReadOnlyChangeLens (toReadOnlyChangeLens . pairWholeChangeLens) . readOnlyPairChangeLens) $ eaPair fa fb

eaToReadOnlyWhole ::
       (EditApplicative f, FullSubjectReader (UpdateReader update), ApplicableUpdate update)
    => f update
    -> f (ROWUpdate (UpdateSubject update))
eaToReadOnlyWhole = eaMap convertReadOnlyChangeLens

eaMapReadOnlyWhole :: EditApplicative f => (a -> b) -> f (ROWUpdate a) -> f (ROWUpdate b)
eaMapReadOnlyWhole ab = eaMapSemiReadOnly $ funcChangeLens ab

class EditApplicative f => FloatingEditApplicative (f :: Type -> Type) where
    eaFloatMap ::
           forall updateA updateB.
           ResourceContext
        -> FloatingChangeLens updateA updateB
        -> f updateA
        -> LifeCycleIO (f updateB)

eaFloatMapReadOnly ::
       forall f updateA updateB. FloatingEditApplicative f
    => ResourceContext
    -> FloatingChangeLens updateA (ReadOnlyUpdate updateB)
    -> f (ReadOnlyUpdate updateA)
    -> LifeCycleIO (f (ReadOnlyUpdate updateB))
eaFloatMapReadOnly rc flens = eaFloatMap rc $ liftReadOnlyFloatingChangeLens flens

instance FloatingEditApplicative (FloatingChangeLens update) where
    eaFloatMap _ ab ua = return $ ab . ua
