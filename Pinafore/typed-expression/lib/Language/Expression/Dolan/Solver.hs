{-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Solver
    ( subtypeSingularType
    , invertedPolarSubtype
    , CrumbleM
    )
where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.Solve
import Language.Expression.Dolan.Solver.Substitute
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = Puzzle ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [SolverBisubstitution ground]
    unifyNegWitnesses ta tb =
        return $ uuLiftNegShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosWitnesses ta tb =
        return $ uuLiftPosShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosNegWitnesses ta tb = fmap MkComposeShim $ puzzleExpressionUnify ta tb
    solveUnifier puzzle = liftCrumbleM $ solvePuzzle puzzle
    unifierPosSubstitute bisubs t = lift $ liftTypeResult $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = lift $ liftTypeResult $ bisubstitutesType bisubs t

instance
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeGroundType ground =>
    SubsumeTypeSystem (DolanTypeSystem ground)
    where
    type Subsumer (DolanTypeSystem ground) = Puzzle ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [SolverBisubstitution ground]
    usubSubsumer ss subsumer = do
        subsumer' <- liftCrumbleM $ applyBisubsToPuzzle ss subsumer
        return $ solverExpressionLiftType subsumer'
    solveSubsumer puzzle = liftCrumbleM $ solvePuzzle puzzle
    subsumerPosSubstitute subs t = lift $ liftTypeResult $ bisubstitutesType subs t
    subsumerNegSubstitute subs t = lift $ liftTypeResult $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = puzzleExpressionUnify tinf tdecl

-- used for simplification, where all vars are fixed
checkSameVar ::
    forall (ground :: GroundTypeKind) wit t.
    wit t ->
    CrumbleM ground t
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
checkSameVar _ = throwExc $ InternalTypeError @ground "expression in type inversion"

evalPuzzleExpression ::
    forall (ground :: GroundTypeKind) a.
    PuzzleExpression ground a ->
    DolanRenameTypeM ground (Puzzle ground a)
evalPuzzleExpression (MkSolverExpression tt (ClosedExpression v)) = return $ fmap v tt
evalPuzzleExpression _ = throwExc $ InternalTypeError "expression in type inversion"

puzzleSubsumeSingular ::
    forall (ground :: GroundTypeKind) polarity a b.
    (IsDolanSubtypeGroundType ground, Is PolarityType polarity) =>
    DolanSingularType ground polarity a ->
    DolanSingularType ground polarity b ->
    DolanRenameTypeM ground (Puzzle ground (DolanPolarShim ground polarity a b))
puzzleSubsumeSingular ta tb =
    case polarityType @polarity of
        PositiveType -> fmap (fmap MkPolarShim) $ puzzleUnifySingular ta tb
        NegativeType -> fmap (fmap MkPolarShim) $ puzzleUnifySingular tb ta

-- used for simplification, where all vars are fixed
subtypeSingularType ::
    forall (ground :: GroundTypeKind) polarity a b.
    (IsDolanSubtypeGroundType ground, Is PolarityType polarity) =>
    DolanSingularType ground polarity a ->
    DolanSingularType ground polarity b ->
    CrumbleM ground (DolanPolarShim ground polarity a b)
subtypeSingularType ta tb = do
    puzzle <- liftToCrumbleM $ puzzleSubsumeSingular ta tb
    (oexpr, _) <- solvePuzzle puzzle
    runExpression checkSameVar oexpr

invertedPolarSubtype ::
    forall (ground :: GroundTypeKind) polarity a b.
    (Is PolarityType polarity, IsDolanSubtypeGroundType ground) =>
    DolanType ground (InvertPolarity polarity) a ->
    DolanType ground polarity b ->
    DolanRenameTypeM ground (Puzzle ground (DolanPolarShim ground polarity a b))
invertedPolarSubtype ta tb = do
    pexpr <-
        case polarityType @polarity of
            PositiveType -> fmap (fmap MkPolarShim) $ puzzleExpressionUnify ta tb
            NegativeType -> fmap (fmap MkPolarShim) $ puzzleExpressionUnify tb ta
    evalPuzzleExpression pexpr
