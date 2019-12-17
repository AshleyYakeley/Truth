module Pinafore.Base.Value where

import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

type PinaforeFunctionValue baseupdate t = UpdateFunction baseupdate (WholeUpdate t)

applyPinaforeFunction ::
       forall baseupdate a b.
       PinaforeFunctionMorphism baseupdate a b
    -> PinaforeFunctionValue baseupdate a
    -> PinaforeFunctionValue baseupdate b
applyPinaforeFunction m va = pinaforeFunctionMorphismUpdateFunction m . contextualiseUpdateFunction va

type PinaforeLensValue baseupdate = EditLens baseupdate

lensFunctionValue ::
       (IsEditUpdate update, FullSubjectReader (UpdateReader update), ApplicableEdit (UpdateEdit update))
    => PinaforeLensValue baseupdate update
    -> PinaforeFunctionValue baseupdate (UpdateSubject update)
lensFunctionValue lens = convertUpdateFunction . editLensFunction lens

applyPinaforeLens ::
       PinaforeLensMorphism baseupdate a b
    -> PinaforeLensValue baseupdate (WholeUpdate (Know a))
    -> PinaforeLensValue baseupdate (WholeUpdate (Know b))
applyPinaforeLens pm val = pmForward pm . contextualiseEditLens val

applyInversePinaforeLens ::
       forall baseupdate a b. (Eq a, Eq b)
    => PinaforeLensMorphism baseupdate a b
    -> PinaforeLensValue baseupdate (WholeUpdate (Know b))
    -> PinaforeLensValue baseupdate (FiniteSetUpdate a)
applyInversePinaforeLens pm val = pinaforeLensMorphismInverseEditLens pm . contextualiseEditLens val

applyInversePinaforeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => IO b
    -> PinaforeLensMorphism baseupdate a b
    -> PinaforeLensValue baseupdate (FiniteSetUpdate b)
    -> PinaforeLensValue baseupdate (FiniteSetUpdate a)
applyInversePinaforeLensSet newb pm val = pinaforeLensMorphismInverseEditLensSet newb pm . contextualiseEditLens val
