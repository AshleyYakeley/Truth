module Pinafore.Language.Type.Storable.Dynamic.Abstract
    ( AbstractDynamicEntityFamily(..)
    , abstractDynamicStorableGroundType
    , abstractDynamicStorableFamilyWitness
    ) where

import Data.Shim
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Storable.Dynamic.DynamicEntity
import Pinafore.Language.Type.Storable.Dynamic.Entity
import Pinafore.Language.Type.Storable.Dynamic.Storability
import Pinafore.Language.Type.Storable.Type
import Shapes

data AbstractDynamicEntityFamily :: FamilyKind where
    MkAbstractDynamicEntityFamily
        :: FullName -> TypeIDType tid -> DynamicTypeSet -> AbstractDynamicEntityFamily DynamicEntity

instance TestHetEquality AbstractDynamicEntityFamily where
    testHetEquality (MkAbstractDynamicEntityFamily _ t1 _) (MkAbstractDynamicEntityFamily _ t2 _) = do
        Refl <- testEquality t1 t2
        return HRefl

instance Eq (AbstractDynamicEntityFamily t) where
    f1 == f2 = isJust $ testHetEquality f1 f2

abstractDynamicStorableFamilyWitness :: IOWitness ('MkWitKind AbstractDynamicEntityFamily)
abstractDynamicStorableFamilyWitness = $(iowitness [t|'MkWitKind AbstractDynamicEntityFamily|])

abstractDynamicStorableGroundType :: FullName -> TypeIDType tid -> DynamicTypeSet -> QGroundType '[] DynamicEntity
abstractDynamicStorableGroundType name tid dts = let
    props = singleGroundProperty storabilityProperty $ dynamicEntityStorability $ Just dts
    in (singleGroundType'
            (MkFamilialType abstractDynamicStorableFamilyWitness $ MkAbstractDynamicEntityFamily name tid dts)
            props $
        exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 SimplePolyGreatestDynamicSupertype
                     dynamicEntityStorableGroundType
                     (functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (member dt dts) de)
                     id
           }
