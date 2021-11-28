module Pinafore.Language.Type.Entity.Closed where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Shapes

data ClosedEntityFamily :: FamilyKind where
    MkClosedEntityFamily
        :: forall (tid :: BigNat).
           Name
        -> TypeIDType tid
        -> ClosedEntityType (Identified tid)
        -> ClosedEntityFamily (Identified tid)

instance TestHetEquality ClosedEntityFamily where
    testHetEquality (MkClosedEntityFamily _ sa ta) (MkClosedEntityFamily _ sb tb) = do
        Refl <- testEquality sa sb
        Refl <- testEquality ta tb
        return HRefl

data ClosedEntityType (t :: Type) where
    NilClosedEntityType :: ClosedEntityType Void
    ConsClosedEntityType
        :: Anchor -> ListType MonoEntityType tl -> ClosedEntityType tt -> ClosedEntityType (Either (HList tl) tt)

instance TestEquality ClosedEntityType where
    testEquality NilClosedEntityType NilClosedEntityType = Just Refl
    testEquality (ConsClosedEntityType a1 l1 t1) (ConsClosedEntityType a2 l2 t2)
        | a1 == a2 = do
            Refl <- testEquality l1 l2
            Refl <- testEquality t1 t2
            Just Refl
    testEquality _ _ = Nothing

instance Show (ClosedEntityType t) where
    show NilClosedEntityType = "nil"
    show (ConsClosedEntityType a tt NilClosedEntityType) = show tt <> " " <> show a
    show (ConsClosedEntityType a tt rest) = show tt <> " " <> show a <> " | " <> show rest

closedEntityTypeEq :: ClosedEntityType t -> Dict (Eq t)
closedEntityTypeEq NilClosedEntityType = Dict
closedEntityTypeEq (ConsClosedEntityType _ t1 tr) =
    case (hListEq monoEntityTypeEq t1, closedEntityTypeEq tr) of
        (Dict, Dict) -> Dict

closedEntityTypeAdapter :: ClosedEntityType t -> EntityAdapter t
closedEntityTypeAdapter NilClosedEntityType = pNone
closedEntityTypeAdapter (ConsClosedEntityType a cc rest) =
    constructorEntityAdapter a (mapListType monoEntityAdapter cc) <+++> closedEntityTypeAdapter rest

closedEntityFamilyWitness :: IOWitness ('MkWitKind ClosedEntityFamily)
closedEntityFamilyWitness = $(iowitness [t|'MkWitKind ClosedEntityFamily|])

closedEntityGroundType :: forall t. ClosedEntityFamily t -> PinaforeGroundType '[] t
closedEntityGroundType fam@(MkClosedEntityFamily name _ _) =
    singleGroundType' (MkFamilyType closedEntityFamilyWitness fam) $ exprShowPrec name

closedEntityFamily :: EntityFamily
closedEntityFamily =
    MkEntityFamily closedEntityFamilyWitness $ \NilListType (MkClosedEntityFamily name _ cet :: _ t) -> let
        epCovaryMap = covarymap
        epEq :: forall (ta :: Type). Arguments (MonoType EntityGroundType) t ta -> Dict (Eq ta)
        epEq NilArguments = closedEntityTypeEq cet
        epAdapter :: forall ta. Arguments MonoEntityType t ta -> EntityAdapter ta
        epAdapter NilArguments = closedEntityTypeAdapter cet
        epShowType = exprShowPrec name
        in Just MkEntityProperties {..}
