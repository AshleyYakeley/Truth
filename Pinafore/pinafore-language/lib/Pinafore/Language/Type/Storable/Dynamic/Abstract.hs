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
    MkAbstractDynamicEntityFamily :: TypeIDType tid -> DynamicTypeSet -> AbstractDynamicEntityFamily DynamicEntity

instance TestHetEquality AbstractDynamicEntityFamily where
    testHetEquality (MkAbstractDynamicEntityFamily t1 _) (MkAbstractDynamicEntityFamily t2 _) = do
        Refl <- testEquality t1 t2
        return HRefl

abstractDynamicStorableFamilyWitness :: IOWitness ('MkWitKind AbstractDynamicEntityFamily)
abstractDynamicStorableFamilyWitness = $(iowitness [t|'MkWitKind AbstractDynamicEntityFamily|])

abstractDynamicStorableGroundType :: FullName -> TypeIDType tid -> DynamicTypeSet -> QGroundType '[] DynamicEntity
abstractDynamicStorableGroundType name tid dts = let
    props = singleGroundProperty storabilityProperty $ dynamicEntityStorability $ Just dts
    in (singleGroundType'
            (MkFamilialType abstractDynamicStorableFamilyWitness $ MkAbstractDynamicEntityFamily tid dts)
            props $
        exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 SimplePolyGreatestDynamicSupertype
                     dynamicEntityStorableGroundType
                     (functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (member dt dts) de)
                     id
           }
