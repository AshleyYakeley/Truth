module Language.Expression.Dolan.Solver.Biconstraint
    ( Biconstraint(..)
    , BiPuzzle
    , atomicToBiPuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.InvertedCombine
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

--import Language.Expression.Dolan.Bisubstitute
type ShimPair :: GroundTypeKind -> Type -> Type -> Type -> Type
type ShimPair ground v a b = (DolanShim ground a v, DolanShim ground v b)

type Biconstraint :: GroundTypeKind -> Type -> Type
data Biconstraint ground t where
    MkBiconstraint
        :: forall (ground :: GroundTypeKind) v na nb ia ib.
           TypeVarT v
        -> DolanType ground 'Positive na
        -> DolanType ground 'Negative nb
        -> InvertedCombinedDolanType ground 'Positive ia
        -> InvertedCombinedDolanType ground 'Negative ib
        -> Biconstraint ground (ShimPair ground v na nb, ShimPair ground v ia ib)

{-
biconstraintBisubstitution :: forall (ground :: GroundTypeKind)  ts is . IsDolanGroundType ground =>
    Biconstraint ground (ts,is) -> ts -> Bisubstitution ground (DolanShim ground) Identity
biconstraintBisubstitution (MkBiconstraint v tp tn _ _) (convp,convn) =
    MkBisubstitution v (return $ MkShimWit tp $ MkPolarMap $ foo convp)(return $ MkShimWit tn $ MkPolarMap $ foo convn)


type Bisubstitution :: GroundTypeKind -> ShimKind Type -> (Type -> Type) -> Type
data Bisubstitution ground shim m =
    forall tv. MkBisubstitution (TypeVarT tv)
                                (m (PShimWit shim (DolanType ground) 'Positive tv))
                                (m (PShimWit shim (DolanType ground) 'Negative tv))
-}
type BiPuzzle :: GroundTypeKind -> Type -> Type
type BiPuzzle ground = Expression (Biconstraint ground)

newBiconstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => AtomicConstraint ground a
    -> BiPuzzle ground a
newBiconstraint (MkAtomicConstraint var PositiveType (NormalFlipType t) _) =
    fmap (fst . fst) $
    varExpression $ MkBiconstraint var t NilDolanType NilInvertedCombinedDolanType NilInvertedCombinedDolanType
newBiconstraint (MkAtomicConstraint var NegativeType (NormalFlipType t) _) =
    fmap (snd . fst) $
    varExpression $ MkBiconstraint var NilDolanType t NilInvertedCombinedDolanType NilInvertedCombinedDolanType
newBiconstraint (MkAtomicConstraint var PositiveType (InvertFlipType t) _) =
    fmap ((\conv -> conv . iJoinR1) . fst . snd) $
    varExpression $
    MkBiconstraint
        var
        NilDolanType
        NilDolanType
        (ConsInvertedCombinedDolanType t NilInvertedCombinedDolanType)
        NilInvertedCombinedDolanType
newBiconstraint (MkAtomicConstraint var NegativeType (InvertFlipType t) _) =
    fmap ((\conv -> iMeetL1 . conv) . snd . snd) $
    varExpression $
    MkBiconstraint
        var
        NilDolanType
        NilDolanType
        NilInvertedCombinedDolanType
        (ConsInvertedCombinedDolanType t NilInvertedCombinedDolanType)

matchBiconstraint ::
       forall (ground :: GroundTypeKind) a b. IsDolanGroundType ground
    => AtomicConstraint ground a
    -> Biconstraint ground b
    -> Maybe (BiPuzzle ground (b, a))
matchBiconstraint (MkAtomicConstraint var PositiveType (NormalFlipType t) _) (MkBiconstraint cv ctp ctn itp itn) = do
    Refl <- testEquality var cv
    return $
        case joinMeetType t ctp of
            MkShimWit ctp' (MkPolarMap conv) ->
                fmap (\((tv, vb), itt) -> (((tv . conv . join2, vb), itt), tv . conv . join1)) $
                varExpression $ MkBiconstraint cv ctp' ctn itp itn
matchBiconstraint (MkAtomicConstraint var NegativeType (NormalFlipType t) _) (MkBiconstraint cv ctp ctn itp itn) = do
    Refl <- testEquality var cv
    return $
        case joinMeetType t ctn of
            MkShimWit ctn' (MkPolarMap conv) ->
                fmap (\((av, vt), itt) -> (((av, meet2 . conv . vt), itt), meet1 . conv . vt)) $
                varExpression $ MkBiconstraint cv ctp ctn' itp itn
matchBiconstraint (MkAtomicConstraint var PositiveType (InvertFlipType t) _) (MkBiconstraint cv ctp ctn itp itn) = do
    Refl <- testEquality var cv
    return $ let
        itp' = ConsInvertedCombinedDolanType t itp
        in fmap (\(ntt, (tv, vb)) -> ((ntt, (tv . join2, vb)), tv . join1)) $
           varExpression $ MkBiconstraint cv ctp ctn itp' itn
matchBiconstraint (MkAtomicConstraint var NegativeType (InvertFlipType t) _) (MkBiconstraint cv ctp ctn itp itn) = do
    Refl <- testEquality var cv
    return $ let
        itn' = ConsInvertedCombinedDolanType t itn
        in fmap (\(ntt, (av, vt)) -> ((ntt, (av, meet2 . vt)), meet1 . vt)) $
           varExpression $ MkBiconstraint cv ctp ctn itp itn'

processConstraint ::
       forall (ground :: GroundTypeKind) t a. IsDolanGroundType ground
    => AtomicConstraint ground t
    -> BiPuzzle ground (t -> a)
    -> BiPuzzle ground a
processConstraint ac = mergeExpressionWitnesses (newBiconstraint ac) (matchBiconstraint ac)

atomicToBiPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => AtomicPuzzle ground a
    -> BiPuzzle ground a
atomicToBiPuzzle (ClosedExpression a) = ClosedExpression a
atomicToBiPuzzle (OpenExpression ac puzzle) = processConstraint ac $ atomicToBiPuzzle puzzle
