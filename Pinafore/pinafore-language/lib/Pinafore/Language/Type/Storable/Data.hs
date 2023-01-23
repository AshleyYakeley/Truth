module Pinafore.Language.Type.Storable.Data
    ( storableDataGroundType
    ) where

import Language.Expression.Dolan
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Storable.Type
import Shapes

data DataStorableFamily :: FamilyKind where
    MkDataStorableFamily
        :: forall (tid :: Nat) k (gt :: k). (Identified tid ~~ gt)
        => TypeIDType tid
        -> SealedStorability gt
        -> DataStorableFamily gt

instance TestHetEquality DataStorableFamily where
    testHetEquality (MkDataStorableFamily ia _) (MkDataStorableFamily ib _) = do
        Refl <- testEquality ia ib
        return HRefl

dataStorableFamilyWitness :: IOWitness ('MkWitKind DataStorableFamily)
dataStorableFamilyWitness = $(iowitness [t|'MkWitKind DataStorableFamily|])

storableDataGroundType ::
       forall tid dv (gt :: DolanVarianceKind dv). (IdentifiedKind tid ~ DolanVarianceKind dv, gt ~~ Identified tid)
    => TypeIDType tid
    -> ListTypeExprShow dv
    -> Storability dv gt
    -> QGroundType dv gt
storableDataGroundType tidsym showType storability@MkStorability {..} =
    MkQGroundType
        { qgtVarianceType = covaryToDolanVarianceType stbKind
        , qgtVarianceMap = covaryToDolanVarianceMap stbKind stbCovaryMap
        , qgtShowType = showType
        , qgtFamilyType =
              MkFamilialType dataStorableFamilyWitness $
              MkDataStorableFamily tidsym $ MkSealedStorability showType storability
        , qgtSubtypeGroup = Nothing
        , qgtProperties = singleGroundProperty storabilityProperty storability
        , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
        }
