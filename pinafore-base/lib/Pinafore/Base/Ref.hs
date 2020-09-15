module Pinafore.Base.Ref where

import Changes.Core
import Pinafore.Base.FunctionMorphism
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes

contextualiseModels :: Model baseupdate -> Model update -> Model (ContextUpdate baseupdate update)
contextualiseModels subx subn =
    tupleModel $ \case
        SelectContext -> subx
        SelectContent -> subn

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

newtype PinaforeRef update = MkPinaforeRef
    { unPinaforeRef :: Model update
    }

pinaforeRefModel :: PinaforeRef update -> Model update
pinaforeRefModel = unPinaforeRef

pinaforeRefWaitUpdates :: PinaforeRef update -> View ()
pinaforeRefWaitUpdates (MkPinaforeRef ref) = viewWaitUpdates ref

instance EditApplicative PinaforeRef where
    eaPure subj = MkPinaforeRef $ constantModel subj
    eaMap lens (MkPinaforeRef sv) = MkPinaforeRef $ mapModel lens sv
    eaPair (MkPinaforeRef sva) (MkPinaforeRef svb) = MkPinaforeRef $ pairModels sva svb

instance FloatingEditApplicative PinaforeRef where
    eaFloatMap rc flens (MkPinaforeRef sub) = fmap MkPinaforeRef $ floatMapModel rc flens sub

contextualisePinaforeRef :: Model baseupdate -> PinaforeRef update -> PinaforeRef (ContextUpdate baseupdate update)
contextualisePinaforeRef basesub (MkPinaforeRef sv) = MkPinaforeRef $ contextualiseModels basesub sv

type PinaforeROWRef a = PinaforeRef (ROWUpdate a)

pinaforeFunctionValueGet :: ResourceContext -> PinaforeROWRef t -> IO t
pinaforeFunctionValueGet rc (MkPinaforeRef sub) = runResource rc sub $ \asub -> aModelRead asub ReadWhole

pinaforeRefPush :: ResourceContext -> PinaforeRef update -> NonEmpty (UpdateEdit update) -> IO Bool
pinaforeRefPush rc (MkPinaforeRef sub) edits =
    runResource rc sub $ \asub -> pushEdit noEditSource $ aModelEdit asub edits

applyPinaforeFunction ::
       forall baseupdate a b.
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate a b
    -> PinaforeROWRef a
    -> PinaforeROWRef b
applyPinaforeFunction basesub m val =
    eaMap (pinaforeFunctionMorphismUpdateFunction m) $
    contextualisePinaforeRef basesub $ eaMap fromReadOnlyRejectingChangeLens val

applyPinaforeLens ::
       forall baseupdate ap aq bp bq.
       Model baseupdate
    -> PinaforeLensMorphism baseupdate ap aq bp bq
    -> PinaforeRef (BiWholeUpdate (Know aq) (Know ap))
    -> PinaforeRef (BiWholeUpdate (Know bp) (Know bq))
applyPinaforeLens basesub pm val = eaMap (pinaforeLensMorphismChangeLens pm) $ contextualisePinaforeRef basesub val

applyInversePinaforeLens ::
       forall baseupdate a bp bq. (Eq a)
    => Model baseupdate
    -> PinaforeLensMorphism baseupdate a a bq bp
    -> PinaforeRef (BiWholeUpdate (Know bp) (Know bq))
    -> PinaforeRef (FiniteSetUpdate a)
applyInversePinaforeLens basesub pm val =
    eaMap (pinaforeLensMorphismInverseChangeLens pm) $ contextualisePinaforeRef basesub val

applyInversePinaforeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => Model baseupdate
    -> PinaforeLensMorphism baseupdate a a b b
    -> PinaforeRef (FiniteSetUpdate b)
    -> PinaforeRef (FiniteSetUpdate a)
applyInversePinaforeLensSet basesub pm val =
    eaMap (pinaforeLensMorphismInverseChangeLensSet pm) $ contextualisePinaforeRef basesub val
