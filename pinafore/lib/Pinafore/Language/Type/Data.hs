module Pinafore.Language.Type.Data where

import Pinafore.Language.TypeSystem.Nonpolar
import Shapes

-- | Structure of a datatype
data PinaforeDataType :: Type -> forall (k :: Type). k -> Type where
    NilDataType :: PinaforeDataType baseupdate None
    ConsDataType
        :: ListType (PinaforeNonpolarType baseupdate '[]) tl
        -> PinaforeDataType baseupdate tt
        -> PinaforeDataType baseupdate (Either (HList tl) tt)

-- | Structural equality
instance TestHetEquality (PinaforeDataType baseupdate) where
    testHetEquality NilDataType NilDataType = return HRefl
    testHetEquality (ConsDataType a1 ar) (ConsDataType b1 br) = do
        Refl <- testEquality a1 b1
        HRefl <- testHetEquality ar br
        return HRefl
    testHetEquality _ _ = Nothing
