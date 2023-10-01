module Language.Expression.Dolan.SubtypeEntry.Group where

import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type SubtypeGroup :: GroundTypeKind -> GroundTypeKind
data SubtypeGroup ground dv gt = MkSubtypeGroup
    { sgWitness :: ground dv gt
    , sgIsSubtype :: ground dv gt -> ground dv gt -> Bool
    }

subtypeGroupTestEquality ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanGroundType ground
    => SubtypeGroup ground dva gta
    -> SubtypeGroup ground dvb gtb
    -> Maybe (dva :~: dvb, gta :~~: gtb)
subtypeGroupTestEquality ta tb = groundTypeTestEquality (sgWitness ta) (sgWitness tb)

type SomeSubtypeGroup :: GroundTypeKind -> Type
data SomeSubtypeGroup ground where
    MkSomeSubtypeGroup :: forall (ground :: GroundTypeKind) dv gt. SubtypeGroup ground dv gt -> SomeSubtypeGroup ground

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Eq (SomeSubtypeGroup ground) where
    MkSomeSubtypeGroup a == MkSomeSubtypeGroup b = isJust $ subtypeGroupTestEquality a b

instance forall (ground :: GroundTypeKind) dv gt. DebugIsDolanGroundType ground => Show (SubtypeGroup ground dv gt) where
    show (MkSubtypeGroup t _) = show t

testEqualitySubtypeGroupTest ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => ground dv gt
    -> ground dv gt
    -> Bool
testEqualitySubtypeGroupTest ta tb = isJust $ groundTypeTestEquality ta tb

singletonSubtypeGroup ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => ground dv gt
    -> SubtypeGroup ground dv gt
singletonSubtypeGroup gt = MkSubtypeGroup gt testEqualitySubtypeGroupTest
