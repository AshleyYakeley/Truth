module Pinafore.Base.Model.ModelProperty where

import Changes.Core
import Shapes

import Pinafore.Base.Know
import Pinafore.Base.Model.LensAttribute
import Pinafore.Base.Model.LensProperty
import Pinafore.Base.Model.Model
import Pinafore.Base.Model.ModelBased

type ModelAttribute ap aq bp bq = ModelBased (StorageLensAttribute ap aq bp bq)

type ModelProperty ap aq bp bq = ModelBased (StorageLensProperty ap aq bp bq)

modelPropertyAttribute :: ModelProperty ap aq bp bq -> ModelAttribute ap aq bp bq
modelPropertyAttribute = mapModelBased slpAttribute

applyModelAttribute ::
    forall ap aq bp bq.
    ModelAttribute ap aq bp bq ->
    WModel (BiWholeUpdate (Know aq) (Know ap)) ->
    WModel (BiWholeUpdate (Know bp) (Know bq))
applyModelAttribute mmod = modelBasedModel mmod $ applyStorageLens

applyInverseModelProperty ::
    forall a bp bq.
    Eq a =>
    ModelProperty a a bq bp ->
    WModel (BiWholeUpdate (Know bp) (Know bq)) ->
    WModel (FiniteSetUpdate a)
applyInverseModelProperty mmod = modelBasedModel mmod $ applyInverseStorageLens

applyInverseModelPropertySet ::
    forall a b.
    (Eq a, Eq b) =>
    ModelProperty a a b b ->
    WModel (FiniteSetUpdate b) ->
    WModel (FiniteSetUpdate a)
applyInverseModelPropertySet mmod = modelBasedModel mmod $ applyInverseStorageLensSet
