module Pinafore.Language.Type.Storable.Dynamic.DynamicEntity
    ( dynamicEntityStorableGroundType
    ) where

import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Storable.Dynamic.Entity
import Pinafore.Language.Type.Storable.Dynamic.Storability
import Pinafore.Language.Type.Storable.Type
import Shapes

dynamicEntityStorableGroundType :: QGroundType '[] DynamicEntity
dynamicEntityStorableGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily DynamicEntity)|]) "DynamicEntity")
        {qgtProperties = singleGroundProperty storabilityProperty $ dynamicEntityStorability $ return Nothing}
