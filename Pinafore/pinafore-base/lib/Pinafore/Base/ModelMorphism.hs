module Pinafore.Base.ModelMorphism where

import Changes.Core
import Pinafore.Base.Know
import Pinafore.Base.ModelBased
import Pinafore.Base.Morphism
import Pinafore.Base.Ref
import Shapes

type ModelMorphism ap aq bp bq = ModelBased (PinaforeLensMorphism ap aq bp bq)

applyModelMorphism ::
       forall ap aq bp bq.
       ModelMorphism ap aq bp bq
    -> WModel (BiWholeUpdate (Know aq) (Know ap))
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
applyModelMorphism mmod = modelBasedModel mmod $ applyPinaforeLens

applyInverseModelMorphism ::
       forall a bp bq. (Eq a)
    => ModelMorphism a a bq bp
    -> WModel (BiWholeUpdate (Know bp) (Know bq))
    -> WModel (FiniteSetUpdate a)
applyInverseModelMorphism mmod = modelBasedModel mmod $ applyInversePinaforeLens

applyInverseModelMorphismSet ::
       forall a b. (Eq a, Eq b)
    => ModelMorphism a a b b
    -> WModel (FiniteSetUpdate b)
    -> WModel (FiniteSetUpdate a)
applyInverseModelMorphismSet mmod = modelBasedModel mmod $ applyInversePinaforeLensSet
