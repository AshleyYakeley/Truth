module Pinafore.Language.Type.Storable.Data
    ( storableDataGroundType
    , dataStorableFamily
    ) where

import Language.Expression.Dolan
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
    -> Storability dv gt
    -> QGroundType dv gt
storableDataGroundType tidsym props@MkStorability {..} =
    MkPinaforeGroundType
        { pgtVarianceType = covaryToDolanVarianceType epKind
        , pgtVarianceMap = covaryToDolanVarianceMap epKind epCovaryMap
        , pgtShowType = epShowType
        , pgtFamilyType =
              MkFamilialType dataStorableFamilyWitness $ MkDataStorableFamily tidsym $ MkSealedStorability props
        , pgtSubtypeGroup = Nothing
        , pgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
        }

dataStorableFamily :: StorableFamily
dataStorableFamily = MkStorableFamily dataStorableFamilyWitness $ \(MkDataStorableFamily _ eprops) -> Just eprops
