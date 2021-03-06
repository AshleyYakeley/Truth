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

contextualisePinaforeRef :: Model baseupdate -> WModel update -> WModel (ContextUpdate baseupdate update)
contextualisePinaforeRef basesub (MkWModel sv) = MkWModel $ contextualiseModels basesub sv

type PinaforeROWRef a = WModel (ROWUpdate a)

applyPinaforeFunction ::
       forall baseupdate a b.
       Model baseupdate
    -> PinaforeFunctionMorphism baseupdate a b
    -> PinaforeROWRef a
    -> PinaforeROWRef b
applyPinaforeFunction basesub m val =
    eaMap (pinaforeFunctionMorphismContextChangeLens m) $
    contextualisePinaforeRef basesub $ eaMap fromReadOnlyRejectingChangeLens val

applyPinaforeLens ::
       forall baseupdate ap aq bp bq.
       Model baseupdate
    -> PinaforeLensMorphism ap aq bp bq baseupdate
    -> WModel (BiWholeUpdate (Know aq) (Know ap))
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
applyPinaforeLens basesub pm val = eaMap (pinaforeLensMorphismChangeLens pm) $ contextualisePinaforeRef basesub val

applyInversePinaforeLens ::
       forall baseupdate a bp bq. (Eq a)
    => Model baseupdate
    -> PinaforeLensMorphism a a bq bp baseupdate
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
    -> WModel (FiniteSetUpdate a)
applyInversePinaforeLens basesub pm val =
    eaMap (pinaforeLensMorphismInverseChangeLens pm) $ contextualisePinaforeRef basesub val

applyInversePinaforeLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => Model baseupdate
    -> PinaforeLensMorphism a a b b baseupdate
    -> WModel (FiniteSetUpdate b)
    -> WModel (FiniteSetUpdate a)
applyInversePinaforeLensSet basesub pm val =
    eaMap (pinaforeLensMorphismInverseChangeLensSet pm) $ contextualisePinaforeRef basesub val
