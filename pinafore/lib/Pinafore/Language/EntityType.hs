module Pinafore.Language.EntityType where

import Pinafore.Base
import Pinafore.Language.Type
import Shapes

data EntityType t where
    SimpleEntityType :: SimpleEntityType t -> EntityType t
    -- PairEntityType :: EntityType ta -> EntityType tb -> EntityType (ta,tb)

instance TestEquality EntityType where
    testEquality (SimpleEntityType t1) (SimpleEntityType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    --testEquality _ _ = Nothing

entityTypeEq :: EntityType t -> Dict (Eq t)
entityTypeEq (SimpleEntityType st) = simpleEntityTypeEq st

entityTypeToType :: IsTypePolarity polarity => EntityType t -> PinaforeTypeF baseedit polarity t
entityTypeToType (SimpleEntityType t) =
    singlePinaforeTypeF $ mkTypeF $ GroundPinaforeSingularType (SimpleEntityPinaforeGroundType t) NilDolanArguments

entityPointAdapter :: HasPinaforePointEdit baseedit => EntityType t -> PointAdapter baseedit t
entityPointAdapter (SimpleEntityType t) = simpleEntityPointAdapter t
