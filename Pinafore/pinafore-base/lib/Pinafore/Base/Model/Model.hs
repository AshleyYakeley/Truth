module Pinafore.Base.Model.Model where

import Changes.Core
import Shapes

import Pinafore.Base.Know
import Pinafore.Base.Model.FunctionAttribute
import Pinafore.Base.Model.LensAttribute
import Pinafore.Base.Model.LensProperty

contextualiseModels :: Model baseupdate -> Model update -> Model (ContextUpdate baseupdate update)
contextualiseModels subx subn =
    tupleModel $ \case
        SelectContext -> subx
        SelectContent -> subn

contextualisePinaforeModel :: Model baseupdate -> WModel update -> WModel (ContextUpdate baseupdate update)
contextualisePinaforeModel basesub (MkWModel sv) = MkWModel $ contextualiseModels basesub sv

type WROWModel a = WModel (ROWUpdate a)

applyStorageFunction ::
    forall baseupdate a b. Model baseupdate -> StorageFunctionAttribute baseupdate a b -> WROWModel a -> WROWModel b
applyStorageFunction basesub m val =
    eaMap (storageFunctionAttributeContextChangeLens m)
        $ contextualisePinaforeModel basesub
        $ eaMap fromReadOnlyRejectingChangeLens val

applyStorageLens ::
    forall baseupdate ap aq bp bq.
    Model baseupdate ->
    StorageLensAttribute ap aq bp bq baseupdate ->
    WModel (BiWholeUpdate (Know aq) (Know ap)) ->
    WModel (BiWholeUpdate (Know bp) (Know bq))
applyStorageLens basesub pm val = eaMap (storageLensAttributeChangeLens pm) $ contextualisePinaforeModel basesub val

applyInverseStorageLens ::
    forall baseupdate a bp bq.
    Eq a =>
    Model baseupdate ->
    StorageLensProperty a a bq bp baseupdate ->
    WModel (BiWholeUpdate (Know bp) (Know bq)) ->
    WModel (FiniteSetUpdate a)
applyInverseStorageLens basesub pm val =
    eaMap (storageLensPropertyInverseChangeLens pm) $ contextualisePinaforeModel basesub val

applyInverseStorageLensSet ::
    forall baseupdate a b.
    (Eq a, Eq b) =>
    Model baseupdate ->
    StorageLensProperty a a b b baseupdate ->
    WModel (FiniteSetUpdate b) ->
    WModel (FiniteSetUpdate a)
applyInverseStorageLensSet basesub pm val =
    eaMap (storageLensPropertyInverseChangeLensSet pm) $ contextualisePinaforeModel basesub val
