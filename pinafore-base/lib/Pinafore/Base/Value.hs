module Pinafore.Base.Value where

import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

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

type ReadOnlyWhole f a = f (ROWUpdate a)

eaPairReadOnlyWhole :: EditApplicative f => ReadOnlyWhole f a -> ReadOnlyWhole f b -> ReadOnlyWhole f (a, b)
eaPairReadOnlyWhole fa fb =
    eaMap (liftReadOnlyChangeLens (toReadOnlyChangeLens . pairWholeChangeLens) . readOnlyPairChangeLens) $ eaPair fa fb

eaToReadOnlyWhole ::
       (EditApplicative f, FullSubjectReader (UpdateReader update), ApplicableUpdate update)
    => f update
    -> ReadOnlyWhole f (UpdateSubject update)
eaToReadOnlyWhole = eaMap convertReadOnlyChangeLens

eaMapReadOnlyWhole :: EditApplicative f => (a -> b) -> ReadOnlyWhole f a -> ReadOnlyWhole f b
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

newtype PinaforeValue update = MkPinaforeValue
    { unPinaforeValue :: Model update
    }

pinaforeValueOpenModel :: PinaforeValue update -> Model update
pinaforeValueOpenModel = unPinaforeValue

instance EditApplicative PinaforeValue where
    eaPure subj = MkPinaforeValue $ constantModel subj
    eaMap lens (MkPinaforeValue sv) = MkPinaforeValue $ mapModel lens sv
    eaPair (MkPinaforeValue sva) (MkPinaforeValue svb) = MkPinaforeValue $ pairModels sva svb

instance FloatingEditApplicative PinaforeValue where
    eaFloatMap rc flens (MkPinaforeValue sub) = fmap MkPinaforeValue $ floatMapModel rc flens sub

contextualisePinaforeValue ::
       Model baseupdate -> PinaforeValue update -> PinaforeValue (ContextUpdate baseupdate update)
contextualisePinaforeValue basesub (MkPinaforeValue sv) = MkPinaforeValue $ contextualiseModels basesub sv

type PinaforeReadOnlyValue t = ReadOnlyWhole PinaforeValue t

pinaforeFunctionValueGet :: ResourceContext -> PinaforeReadOnlyValue t -> IO t
pinaforeFunctionValueGet rc (MkPinaforeValue sub) = runResource rc sub $ \asub -> aModelRead asub ReadWhole

pinaforeValuePush :: ResourceContext -> PinaforeValue update -> NonEmpty (UpdateEdit update) -> IO Bool
pinaforeValuePush rc (MkPinaforeValue sub) edits =
    runResource rc sub $ \asub -> pushEdit noEditSource $ aModelEdit asub edits

applyPinaforeFunction ::
       forall baseupdate a b.
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate a b
    -> PinaforeReadOnlyValue a
    -> PinaforeReadOnlyValue b
applyPinaforeFunction basesub m val =
    eaMap (pinaforeFunctionMorphismUpdateFunction m) $
    contextualisePinaforeValue basesub $ eaMap fromReadOnlyRejectingChangeLens val

applyPinaforeLens ::
       forall baseupdate a b.
       Model baseupdate
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeValue (WholeUpdate (Know a))
    -> PinaforeValue (WholeUpdate (Know b))
applyPinaforeLens basesub pm val = eaMap (pmForward pm) $ contextualisePinaforeValue basesub val

applyInversePinaforeLens ::
       forall baseupdate a b. (Eq a, Eq b)
    => Model baseupdate
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeValue (WholeUpdate (Know b))
    -> PinaforeValue (FiniteSetUpdate a)
applyInversePinaforeLens basesub pm val =
    eaMap (pinaforeLensMorphismInverseChangeLens pm) $ contextualisePinaforeValue basesub val

applyInversePinaforeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => Model baseupdate
    -> IO b
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeValue (FiniteSetUpdate b)
    -> PinaforeValue (FiniteSetUpdate a)
applyInversePinaforeLensSet basesub newb pm val =
    eaMap (pinaforeLensMorphismInverseChangeLensSet newb pm) $ contextualisePinaforeValue basesub val
