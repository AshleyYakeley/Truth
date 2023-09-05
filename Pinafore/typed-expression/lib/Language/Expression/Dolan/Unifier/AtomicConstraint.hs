module Language.Expression.Dolan.Unifier.AtomicConstraint where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.WholeConstraint
import Shapes

type AtomicConstraint :: GroundTypeKind -> Type -> Type
data AtomicConstraint ground t where
    MkAtomicConstraint
        :: forall (ground :: GroundTypeKind) polarity tv t.
           TypeVarT tv
        -> PolarityType polarity
        -> FlipType ground polarity t
        -> Bool
        -> AtomicConstraint ground (PolarMapType (DolanShim ground) polarity t tv)

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (AtomicConstraint ground t) where
    show (MkAtomicConstraint var PositiveType (NormalFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " :> " <> showDolanType wt <> " [+]"
    show (MkAtomicConstraint var NegativeType (NormalFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " <: " <> showDolanType wt <> " [-]"
    show (MkAtomicConstraint var PositiveType (InvertFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " :> " <> showDolanType wt <> " [- INV]"
    show (MkAtomicConstraint var NegativeType (InvertFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " <: " <> showDolanType wt <> " [+ INV]"

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (AtomicConstraint ground) where
    allConstraint = Dict

mkAtomicConstraint ::
       forall (ground :: GroundTypeKind) polarity tv t. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> FlipType ground polarity t
    -> AtomicConstraint ground (PolarMapType (DolanShim ground) polarity t tv)
mkAtomicConstraint var ft = MkAtomicConstraint var representative ft (occursInFlipType var ft)

leAtomicConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity p
    -> AtomicConstraint ground (DolanShim ground tv p)
leAtomicConstraint var pt =
    case polarityType @polarity of
        PositiveType -> mkAtomicConstraint var (InvertFlipType pt)
        NegativeType -> mkAtomicConstraint var (NormalFlipType pt)

geAtomicConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity p
    -> AtomicConstraint ground (DolanShim ground p tv)
geAtomicConstraint var pt =
    case polarityType @polarity of
        PositiveType -> mkAtomicConstraint var (NormalFlipType pt)
        NegativeType -> mkAtomicConstraint var (InvertFlipType pt)

isPureAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => AtomicConstraint ground a
    -> Maybe a
isPureAtomicConstraint (MkAtomicConstraint depvar pol (NormalFlipType tw) _) =
    withRepresentative pol $ do
        MkShimWit t (MkPolarMap conv) <- dolanToMaybeTypeShim tw
        case t of
            VarDolanSingularType v -> do
                Refl <- testEquality v depvar
                return conv
            _ -> Nothing
isPureAtomicConstraint _ = Nothing

atomicWholeConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => AtomicConstraint ground a
    -> WholeConstraintShim ground a
atomicWholeConstraint (MkAtomicConstraint v PositiveType ft _) =
    unNegShimWit (varDolanShimWit v) $ \vt vconv ->
        MkShimWit (MkWholeConstraint ft (NormalFlipType vt)) $ MkCatDual $ \conv -> vconv . conv
atomicWholeConstraint (MkAtomicConstraint v NegativeType ft _) =
    unPosShimWit (varDolanShimWit v) $ \vt vconv ->
        MkShimWit (MkWholeConstraint (NormalFlipType vt) ft) $ MkCatDual $ \conv -> conv . vconv
