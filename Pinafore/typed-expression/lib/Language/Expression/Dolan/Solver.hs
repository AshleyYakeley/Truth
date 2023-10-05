{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Solver
    ( subtypeSingularType
    , invertedPolarSubtype
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.Solver
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

forwardsShimWit ::
       forall (pshim :: PolyShimKind) w polarity t. (Is PolarityType polarity)
    => ShimWit (PolarShim (PolyMapT Isomorphism pshim Type) polarity) w t
    -> ShimWit (PolarShim (pshim Type) polarity) w t
forwardsShimWit =
    case polarityType @polarity of
        PositiveType -> \(MkShimWit w (MkPolarShim (MkPolyMapT conv))) -> MkShimWit w $ MkPolarShim $ isoForwards conv
        NegativeType -> \(MkShimWit w (MkPolarShim (MkPolyMapT conv))) -> MkShimWit w $ MkPolarShim $ isoForwards conv

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = Puzzle ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [SolverBisubstitution ground]
    unifyNegWitnesses ta tb =
        return $ uuLiftNegShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosWitnesses ta tb =
        return $ uuLiftPosShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosNegWitnesses ta tb = fmap MkComposeShim $ puzzleExpressionUnify ta tb
    solveUnifier = solvePuzzle
    unifierPosSubstitute bisubs t =
        lift $ fmap (forwardsShimWit @(DolanPolyShim ground)) $ runTypeResult $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t =
        lift $ fmap (forwardsShimWit @(DolanPolyShim ground)) $ runTypeResult $ bisubstitutesType bisubs t

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SubsumeTypeSystem (DolanTypeSystem ground) where
    type Subsumer (DolanTypeSystem ground) = Puzzle ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [SolverBisubstitution ground]
    usubSubsumer ss subsumer = do
        subsumer' <- runCrumbleM $ bisubstitutesPuzzle ss subsumer
        return $ solverExpressionLiftType subsumer'
    solveSubsumer = solvePuzzle
    subsumerPosSubstitute subs t =
        lift $ fmap (forwardsShimWit @(DolanPolyShim ground)) $ runTypeResult $ bisubstitutesType subs t
    subsumerNegSubstitute subs t =
        lift $ fmap (forwardsShimWit @(DolanPolyShim ground)) $ runTypeResult $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = puzzleExpressionUnify tinf tdecl

-- used for simplification, where all vars are fixed
checkSameVar ::
       forall (ground :: GroundTypeKind) wit t. IsDolanSubtypeGroundType ground
    => wit t
    -> DolanTypeCheckM ground t
{-
checkSameVar (MkAtomicConstraint va polwit (NormalFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) )
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iJoinL1
            NegativeType -> iMeetR1
checkSameVar (MkAtomicConstraint va polwit (InvertFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) )
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iMeetL1
            NegativeType -> iJoinR1
-}
checkSameVar _ = empty

evalPuzzleExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => PuzzleExpression ground a
    -> DolanTypeCheckM ground (Puzzle ground a)
evalPuzzleExpression (MkSolverExpression tt (ClosedExpression v)) = return $ fmap v tt
evalPuzzleExpression _ = empty

puzzleSubsumeSingular ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> DolanTypeCheckM ground (Puzzle ground (DolanPolarShim ground polarity a b))
puzzleSubsumeSingular ta tb =
    case polarityType @polarity of
        PositiveType -> fmap (fmap MkPolarShim) $ puzzleUnifySingular ta tb
        NegativeType -> fmap (fmap MkPolarShim) $ puzzleUnifySingular tb ta

-- used for simplification, where all vars are fixed
subtypeSingularType ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> DolanTypeCheckM ground (DolanPolarShim ground polarity a b)
subtypeSingularType ta tb = do
    puzzle <- puzzleSubsumeSingular ta tb
    oexpr <- rigidSolvePuzzle puzzle
    solveExpression checkSameVar oexpr

invertedPolarSubtype ::
       forall (ground :: GroundTypeKind) polarity a b. (Is PolarityType polarity, IsDolanSubtypeGroundType ground)
    => DolanType ground (InvertPolarity polarity) a
    -> DolanType ground polarity b
    -> DolanTypeCheckM ground (Puzzle ground (DolanPolarShim ground polarity a b))
invertedPolarSubtype ta tb = do
    pexpr <-
        case polarityType @polarity of
            PositiveType -> fmap (fmap MkPolarShim) $ puzzleExpressionUnify ta tb
            NegativeType -> fmap (fmap MkPolarShim) $ puzzleExpressionUnify tb ta
    evalPuzzleExpression pexpr
