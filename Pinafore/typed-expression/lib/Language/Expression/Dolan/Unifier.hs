{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Unifier
    ( unifierSubtypeConversionAsGeneralAs
    , invertType
    , subtypeSingularType
    , invertedPolarSubtype
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Crumble
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.Solver
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Variance
import Shapes

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = Puzzle ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    unifyNegWitnesses ta tb =
        return $ uuLiftNegShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosWitnesses ta tb =
        return $ uuLiftPosShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosNegWitnesses ta tb = pure $ MkComposeShim $ puzzleExpressionUnify ta tb
    solveUnifier = solvePuzzle
    unifierPosSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SubsumeTypeSystem (DolanTypeSystem ground) where
    type Subsumer (DolanTypeSystem ground) = Puzzle ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    usubSubsumer ss subsumer = do
        subsumer' <- lift $ runUnifierM $ bisubstitutesPuzzle ss subsumer
        return $ solverExpressionLiftType subsumer'
    solveSubsumer = solvePuzzle
    subsumerPosSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumerNegSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = return $ puzzleExpressionUnify tinf tdecl

-- used for simplification, where all vars are fixed
checkSameVar ::
       forall (ground :: GroundTypeKind) wit t. IsDolanSubtypeGroundType ground
    => wit t
    -> DolanTypeCheckM ground t
{-
checkSameVar (MkAtomicConstraint va polwit (NormalFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) _)
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iJoinL1
            NegativeType -> iMeetR1
checkSameVar (MkAtomicConstraint va polwit (InvertFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) _)
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
    -> Puzzle ground (DolanPolarMap ground polarity a b)
puzzleSubsumeSingular ta tb =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ puzzleUnifySingular ta tb
        NegativeType -> fmap MkPolarMap $ puzzleUnifySingular tb ta

-- used for simplification, where all vars are fixed
subtypeSingularType ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> DolanTypeCheckM ground (DolanPolarMap ground polarity a b)
subtypeSingularType ta tb = do
    oexpr <- rigidSolvePuzzle $ puzzleSubsumeSingular ta tb
    solveExpression checkSameVar oexpr

invertedPolarSubtype ::
       forall (ground :: GroundTypeKind) polarity a b. (Is PolarityType polarity, IsDolanSubtypeGroundType ground)
    => DolanType ground (InvertPolarity polarity) a
    -> DolanType ground polarity b
    -> DolanTypeCheckM ground (Puzzle ground (DolanPolarMap ground polarity a b))
invertedPolarSubtype ta tb =
    evalPuzzleExpression $
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ puzzleExpressionUnify @ground ta tb
        NegativeType -> fmap MkPolarMap $ puzzleExpressionUnify tb ta

{-
unifySubtypeContext' ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => DolanSubtypeContext ground (Puzzle ground)
unifySubtypeContext' = MkSubtypeContext puzzleUnify solverLiftValueExpression

{-
type UnifierSolver :: GroundTypeKind -> Type -> Type
type UnifierSolver ground = Solver ground (AtomicConstraint ground)

subtypeConversionAsGeneralAs ::
       forall (ground :: GroundTypeKind) solver (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
       (IsDolanSubtypeGroundType ground, WrappedApplicative solver, WAInnerM solver ~ DolanTypeCheckM ground)
    => (forall a. solver a -> WAInnerM solver Bool)
    -> DolanSubtypeContext ground solver
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> DolanM ground Bool


runCheckUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierSolver ground a
    -> DolanTypeCheckM ground Bool
runCheckUnifier us =
    altIs $ do
        MkSolverExpression expr _ <- runSolver us
        _ <- solveUnifier @(DolanTypeSystem ground) expr
        return ()

-}


runCheckUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground Bool
runCheckUnifier puzzle =
    altIs $ do
        _ <- solvePuzzle puzzle
        return ()
-}
unifierSubtypeConversionAsGeneralAs ::
       forall (ground :: GroundTypeKind) (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
       IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> DolanM ground Bool
unifierSubtypeConversionAsGeneralAs _ _ = return $ error "NYI: unifierSubtypeConversionAsGeneralAs"
    --- = subtypeConversionAsGeneralAs runCheckUnifier unifySubtypeContext'
