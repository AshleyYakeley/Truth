module Pinafore.Language.Type.Entity.Closed where

import Language.Expression.Dolan
import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Shapes

data ClosedEntityFamily :: FamilyKind where
    MkClosedEntityFamily
        :: forall (tid :: TNatural) k (gt :: k). (Identified tid ~~ gt)
        => TypeIDType tid
        -> SealedEntityProperties gt
        -> ClosedEntityFamily gt

instance TestHetEquality ClosedEntityFamily where
    testHetEquality (MkClosedEntityFamily ia _) (MkClosedEntityFamily ib _) = do
        Refl <- testEquality ia ib
        return HRefl

closedEntityFamilyWitness :: IOWitness ('MkWitKind ClosedEntityFamily)
closedEntityFamilyWitness = $(iowitness [t|'MkWitKind ClosedEntityFamily|])

closedEntityGroundType ::
       forall tid dv (gt :: DolanVarianceKind dv). (IdentifiedKind tid ~ DolanVarianceKind dv, gt ~~ Identified tid)
    => TypeIDType tid
    -> EntityProperties dv gt
    -> PinaforeGroundType dv gt
closedEntityGroundType tidsym props@MkEntityProperties {..} =
    MkPinaforeGroundType
        { pgtVarianceType = covaryToDolanVarianceType epKind
        , pgtVarianceMap = covaryToDolanVarianceMap epKind epCovaryMap
        , pgtShowType = epShowType
        , pgtFamilyType =
              MkFamilyType closedEntityFamilyWitness $ MkClosedEntityFamily tidsym $ MkSealedEntityProperties props
        , pgtGreatestDynamicSupertype = \_ -> Nothing
        }

closedEntityFamily :: EntityFamily
closedEntityFamily = MkEntityFamily closedEntityFamilyWitness $ \(MkClosedEntityFamily _ eprops) -> Just eprops
