module Pinafore.Base.Model.ModelProperty where

import Changes.Core
import Pinafore.Base.Know
import Pinafore.Base.Model.Model
import Pinafore.Base.Model.ModelBased
import Pinafore.Base.Model.Property
import Shapes

type ModelProperty ap aq bp bq = ModelBased (StorageLensProperty ap aq bp bq)

applyModelProperty ::
       forall ap aq bp bq.
       ModelProperty ap aq bp bq
    -> WModel (BiWholeUpdate (Know aq) (Know ap))
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
applyModelProperty mmod = modelBasedModel mmod $ applyStorageLens

applyInverseModelProperty ::
       forall a bp bq. (Eq a)
    => ModelProperty a a bq bp
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
    -> WModel (FiniteSetUpdate a)
applyInverseModelProperty mmod = modelBasedModel mmod $ applyInverseStorageLens

applyInverseModelPropertySet ::
       forall a b. (Eq a, Eq b)
    => ModelProperty a a b b
    -> WModel (FiniteSetUpdate b)
    -> WModel (FiniteSetUpdate a)
applyInverseModelPropertySet mmod = modelBasedModel mmod $ applyInverseStorageLensSet
