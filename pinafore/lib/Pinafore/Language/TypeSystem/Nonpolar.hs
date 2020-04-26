module Pinafore.Language.TypeSystem.Nonpolar
    ( PinaforeNonpolarType
    , nonpolarToPinaforeType
    , pinaforeTypeToNonpolar
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.UVar
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

newtype AnyPolarity (w :: k -> Type) (polarity :: Polarity) (a :: k) =
    MkAnyPolarity (w a)

instance TestEquality w => TestEquality (AnyPolarity w polarity) where
    testEquality (MkAnyPolarity ta) (MkAnyPolarity tb) = testEquality ta tb

type NonpolarArgument (w :: Type -> Type) (sv :: Variance) = SingleArgument sv (AnyPolarity w) 'Positive

data PinaforeNonpolarType (baseupdate :: Type) (dv :: DolanVariance) (t :: DolanVarianceKind dv) where
    GroundPinaforeNonpolarType :: PinaforeGroundType baseupdate dv t -> PinaforeNonpolarType baseupdate dv t
    ApplyPinaforeNonpolarType
        :: VarianceType sv
        -> PinaforeNonpolarType baseupdate (sv ': dv) f
        -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv a
        -> PinaforeNonpolarType baseupdate dv (f a)
    VarPinaforeNonpolarType :: SymbolType name -> PinaforeNonpolarType baseupdate '[] (UVar name)

nonpolarToPinaforeType ::
       Is PolarityType polarity => PinaforeNonpolarType baseupdate '[] t -> PinaforeShimWit baseupdate polarity t
nonpolarToPinaforeType (GroundPinaforeNonpolarType _gt) = undefined
nonpolarToPinaforeType (ApplyPinaforeNonpolarType _svt _tf _ta) = undefined MkAnyPolarity
nonpolarToPinaforeType (VarPinaforeNonpolarType _n) = undefined

pinaforeTypeToNonpolar :: PinaforeType baseupdate polarity t -> Maybe (AnyW (PinaforeNonpolarType baseupdate '[]))
pinaforeTypeToNonpolar = undefined

pinaforeNonpolarArgTypeTestEquality ::
       forall baseupdate sv a b.
       VarianceType sv
    -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv a
    -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv b
    -> Maybe (a :~: b)
pinaforeNonpolarArgTypeTestEquality CovarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality ContravarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality RangevarianceType = testEquality

pinaforeNonpolarTypeTestEquality ::
       forall baseupdate dva ta dvb tb.
       PinaforeNonpolarType baseupdate dva ta
    -> PinaforeNonpolarType baseupdate dvb tb
    -> Maybe (dva :~: dvb, ta :~~: tb)
pinaforeNonpolarTypeTestEquality (GroundPinaforeNonpolarType ta) (GroundPinaforeNonpolarType tb) = do
    (Refl, HRefl) <- pinaforeGroundTypeTestEquality ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (ApplyPinaforeNonpolarType sva fa ta) (ApplyPinaforeNonpolarType svb fb tb) = do
    Refl <- testEquality sva svb
    (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality fa fb
    Refl <- pinaforeNonpolarArgTypeTestEquality @baseupdate sva ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (VarPinaforeNonpolarType na) (VarPinaforeNonpolarType nb) = do
    Refl <- testEquality na nb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality _ _ = Nothing

instance TestEquality (PinaforeNonpolarType baseupdate '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality ta tb
        return Refl
