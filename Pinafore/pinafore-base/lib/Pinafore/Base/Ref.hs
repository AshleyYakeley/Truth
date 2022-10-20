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

contextualisePinaforeModel :: Model baseupdate -> WModel update -> WModel (ContextUpdate baseupdate update)
contextualisePinaforeModel basesub (MkWModel sv) = MkWModel $ contextualiseModels basesub sv

type WROWModel a = WModel (ROWUpdate a)

applyStorageFunction ::
       forall baseupdate a b. Model baseupdate -> StorageFunctionMorphism baseupdate a b -> WROWModel a -> WROWModel b
applyStorageFunction basesub m val =
    eaMap (storageFunctionMorphismContextChangeLens m) $
    contextualisePinaforeModel basesub $ eaMap fromReadOnlyRejectingChangeLens val

applyStorageLens ::
       forall baseupdate ap aq bp bq.
       Model baseupdate
    -> StorageLensMorphism ap aq bp bq baseupdate
    -> WModel (BiWholeUpdate (Know aq) (Know ap))
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
applyStorageLens basesub pm val = eaMap (storageLensMorphismChangeLens pm) $ contextualisePinaforeModel basesub val

applyInverseStorageLens ::
       forall baseupdate a bp bq. (Eq a)
    => Model baseupdate
    -> StorageLensMorphism a a bq bp baseupdate
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
    -> WModel (FiniteSetUpdate a)
applyInverseStorageLens basesub pm val =
    eaMap (storageLensMorphismInverseChangeLens pm) $ contextualisePinaforeModel basesub val

applyInverseStorageLensSet ::
       forall baseupdate a b. (Eq a, Eq b)
    => Model baseupdate
    -> StorageLensMorphism a a b b baseupdate
    -> WModel (FiniteSetUpdate b)
    -> WModel (FiniteSetUpdate a)
applyInverseStorageLensSet basesub pm val =
    eaMap (storageLensMorphismInverseChangeLensSet pm) $ contextualisePinaforeModel basesub val
