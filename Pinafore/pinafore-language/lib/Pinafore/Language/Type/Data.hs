module Pinafore.Language.Type.Data where

import Language.Expression.Dolan
import Pinafore.Language.Type.Ground
import Shapes

type PinaforeNonpolarType :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type PinaforeNonpolarType = NonpolarDolanType PinaforeGroundType

-- | Structure of a datatype
data PinaforeDataType :: forall (k :: Type). k -> Type where
    NilDataType :: PinaforeDataType None
    ConsDataType
        :: ListType (PinaforeNonpolarType '[]) tl -> PinaforeDataType tt -> PinaforeDataType (Either (HList tl) tt)

-- | Structural equality
instance TestHetEquality PinaforeDataType where
    testHetEquality NilDataType NilDataType = return HRefl
    testHetEquality (ConsDataType a1 ar) (ConsDataType b1 br) = do
        Refl <- testEquality a1 b1
        HRefl <- testHetEquality ar br
        return HRefl
    testHetEquality _ _ = Nothing
