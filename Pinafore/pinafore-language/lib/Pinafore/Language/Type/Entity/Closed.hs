module Pinafore.Language.Type.Entity.Closed where

import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Shapes

data ClosedEntityFamily :: FamilyKind where
    MkClosedEntityFamily
        :: forall (tid :: BigNat) k (t :: k). (IdentifiedKind tid ~ Type, Identified tid ~~ t)
        => TypeIDType tid
        -> SealedEntityProperties t
        -> ClosedEntityFamily t

instance TestHetEquality ClosedEntityFamily where
    testHetEquality (MkClosedEntityFamily ia _) (MkClosedEntityFamily ib _) = do
        Refl <- testEquality ia ib
        return HRefl

closedEntityFamilyWitness :: IOWitness ('MkWitKind ClosedEntityFamily)
closedEntityFamilyWitness = $(iowitness [t|'MkWitKind ClosedEntityFamily|])

closedEntityGroundType :: forall t. ClosedEntityFamily t -> PinaforeGroundType '[] t
closedEntityGroundType fam@(MkClosedEntityFamily _ (MkSealedEntityProperties eprops)) =
    singleGroundType' (MkFamilyType closedEntityFamilyWitness fam) $
    case epKind eprops of
        NilListType -> epShowType eprops

closedEntityFamily :: EntityFamily
closedEntityFamily = MkEntityFamily closedEntityFamilyWitness $ \(MkClosedEntityFamily _ eprops) -> Just eprops
