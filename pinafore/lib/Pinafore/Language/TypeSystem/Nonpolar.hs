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

instance TestEquality (PinaforeNonpolarType baseupdate '[]) where
    testEquality _ _ = undefined
