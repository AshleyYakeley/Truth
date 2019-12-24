module Pinafore.Base.Value where

import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

contextualiseSubscribers :: Subscriber baseupdate -> Subscriber update -> Subscriber (ContextUpdate baseupdate update)
contextualiseSubscribers subx subn =
    tupleSubscriber $ \case
        SelectContext -> subx
        SelectContent -> subn

class EditApplicative (f :: Type -> Type) where
    eaPure ::
           forall update. SubjectReader (UpdateReader update)
        => UpdateSubject update
        -> f (ReadOnlyUpdate update)
    eaMap :: forall updateA updateB. EditLens updateA updateB -> f updateA -> f updateB
    eaPair :: forall updateA updateB. f updateA -> f updateB -> f (PairUpdate updateA updateB)

instance EditApplicative (EditLens update) where
    eaPure subj = updateFunctionToEditLens $ constUpdateFunction subj
    eaMap lens v = lens . v
    eaPair = pairCombineEditLenses

eaMapReadOnly ::
       forall f updateA updateB. EditApplicative f
    => UpdateFunction updateA updateB
    -> f (ReadOnlyUpdate updateA)
    -> f (ReadOnlyUpdate updateB)
eaMapReadOnly uf = eaMap $ updateFunctionToEditLens $ uf . fromReadOnlyUpdateFunction

type ReadOnlyWhole f a = f (ReadOnlyUpdate (WholeUpdate a))

eaPairReadOnlyWhole :: EditApplicative f => ReadOnlyWhole f a -> ReadOnlyWhole f b -> ReadOnlyWhole f (a, b)
eaPairReadOnlyWhole fa fb =
    eaMap (updateFunctionToEditLens $ pairWholeUpdateFunction . readOnlyPairUpdateFunction) $ eaPair fa fb

eaToReadOnlyWhole ::
       ( EditApplicative f
       , IsEditUpdate update
       , FullSubjectReader (UpdateReader update)
       , ApplicableEdit (UpdateEdit update)
       )
    => f update
    -> ReadOnlyWhole f (UpdateSubject update)
eaToReadOnlyWhole = eaMap (updateFunctionToEditLens convertUpdateFunction)

eaMapReadOnlyWhole :: EditApplicative f => (a -> b) -> ReadOnlyWhole f a -> ReadOnlyWhole f b
eaMapReadOnlyWhole ab = eaMapReadOnly $ funcUpdateFunction ab

newtype PinaforeValue update = MkPinaforeValue
    { unPinaforeValue :: Subscriber update
    }

instance EditApplicative PinaforeValue where
    eaPure subj = MkPinaforeValue $ constantSubscriber subj
    eaMap lens (MkPinaforeValue sv) = MkPinaforeValue $ mapPureSubscriber lens sv
    eaPair (MkPinaforeValue sva) (MkPinaforeValue svb) = MkPinaforeValue $ pairSubscribers sva svb

contextualisePinaforeValue ::
       Subscriber baseupdate -> PinaforeValue update -> PinaforeValue (ContextUpdate baseupdate update)
contextualisePinaforeValue basesub (MkPinaforeValue sv) = MkPinaforeValue $ contextualiseSubscribers basesub sv

type PinaforeReadOnlyValue t = ReadOnlyWhole PinaforeValue t

pinaforeFunctionValueGet :: PinaforeReadOnlyValue t -> IO t
pinaforeFunctionValueGet (MkPinaforeValue sv) =
    case subscriberObject sv of
        MkResource rr MkAnObject {..} -> runResourceRunnerWith rr $ \run -> run $ objRead ReadWhole

pinaforeValuePush :: PinaforeValue update -> NonEmpty (UpdateEdit update) -> IO Bool
pinaforeValuePush (MkPinaforeValue sv) edits =
    case subscriberObject sv of
        MkResource rr MkAnObject {..} -> runResourceRunnerWith rr $ \run -> run $ pushEdit noEditSource $ objEdit edits

applyPinaforeFunction ::
       forall baseupdate a b.
       Subscriber baseupdate
    -> PinaforeFunctionMorphism baseupdate a b
    -> PinaforeReadOnlyValue a
    -> PinaforeReadOnlyValue b
applyPinaforeFunction basesub m val =
    eaMap (updateFunctionToEditLens $ pinaforeFunctionMorphismUpdateFunction m) $
    contextualisePinaforeValue basesub $ eaMap fromReadOnlyRejectingEditLens val

applyPinaforeLens ::
       forall baseupdate a b.
       Subscriber baseupdate
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeValue (WholeUpdate (Know a))
    -> PinaforeValue (WholeUpdate (Know b))
applyPinaforeLens basesub pm val = eaMap (pmForward pm) $ contextualisePinaforeValue basesub val

applyInversePinaforeLens ::
       forall baseupdate a b. (Eq a, Eq b)
    => Subscriber baseupdate
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeValue (WholeUpdate (Know b))
    -> PinaforeValue (FiniteSetUpdate a)
applyInversePinaforeLens basesub pm val =
    eaMap (pinaforeLensMorphismInverseEditLens pm) $ contextualisePinaforeValue basesub val

applyInversePinaforeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => Subscriber baseupdate
    -> IO b
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeValue (FiniteSetUpdate b)
    -> PinaforeValue (FiniteSetUpdate a)
applyInversePinaforeLensSet basesub newb pm val =
    eaMap (pinaforeLensMorphismInverseEditLensSet newb pm) $ contextualisePinaforeValue basesub val
