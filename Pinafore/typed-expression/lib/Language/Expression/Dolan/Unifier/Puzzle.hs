module Language.Expression.Dolan.Unifier.Puzzle where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Piece
import Language.Expression.Dolan.Unifier.UnifierM
import Shapes

-- replaces DolanUnifier
type Puzzle :: GroundTypeKind -> Type -> Type
type Puzzle ground = Expression (PuzzlePiece ground)

puzzlePiece :: forall (ground :: GroundTypeKind) a. PuzzlePiece ground a -> Puzzle ground a
puzzlePiece piece = varExpression piece

puzzleUnify ::
       forall (ground :: GroundTypeKind) pola polb a b. (Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> Puzzle ground (DolanShim ground a b)
puzzleUnify ta tb = let
    fta =
        case polarityType @pola of
            PositiveType -> NormalFlipType ta
            NegativeType -> InvertFlipType ta
    ftb =
        case polarityType @polb of
            PositiveType -> InvertFlipType tb
            NegativeType -> NormalFlipType tb
    in puzzlePiece $ MkPuzzlePiece fta ftb

-- replaces DolanUnifierExpression
type PuzzleExpression :: GroundTypeKind -> Type -> Type
type PuzzleExpression ground = DolanSolverExpression ground (PuzzlePiece ground)

puzzleExpressionUnify ::
       forall (ground :: GroundTypeKind) pola polb a b. (Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> PuzzleExpression ground (DolanShim ground a b)
puzzleExpressionUnify ta tb = solverExpressionLiftType $ puzzleUnify ta tb

puzzleUnifySingular ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> Puzzle ground (DolanShim ground a b)
puzzleUnifySingular ta tb =
    fmap (\conv -> iJoinMeetL1 @_ @polb . conv . iJoinMeetR1 @_ @pola) $
    puzzleUnify (singleDolanType ta) (singleDolanType tb)

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground (DolanShim ground) (UnifierM ground)

bisubstituteConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => UnifierBisubstitution ground
    -> PuzzlePiece ground a
    -> UnifierM ground (Puzzle ground a)
bisubstituteConstraint bisub (MkPuzzlePiece (NormalFlipType ta) (NormalFlipType tb)) = do
    MkShimWit ta' (MkPolarMap conva) <- bisubstituteType bisub ta
    MkShimWit tb' (MkPolarMap convb) <- bisubstituteType bisub tb
    return $
        fmap (\conv -> convb . conv . conva) $ varExpression $ MkPuzzlePiece (NormalFlipType ta') (NormalFlipType tb')
bisubstituteConstraint bisub (MkPuzzlePiece (NormalFlipType ta) (InvertFlipType tb)) = do
    MkShimWit ta' (MkPolarMap conva) <- bisubstituteType bisub ta
    return $ fmap (\conv -> conv . conva) $ varExpression $ MkPuzzlePiece (NormalFlipType ta') (InvertFlipType tb)
bisubstituteConstraint bisub (MkPuzzlePiece (InvertFlipType ta) (NormalFlipType tb)) = do
    MkShimWit tb' (MkPolarMap convb) <- bisubstituteType bisub tb
    return $ fmap (\conv -> convb . conv) $ varExpression $ MkPuzzlePiece (InvertFlipType ta) (NormalFlipType tb')
bisubstituteConstraint _bisub (MkPuzzlePiece (InvertFlipType ta) (InvertFlipType tb)) = do
    return $ varExpression $ MkPuzzlePiece (InvertFlipType ta) (InvertFlipType tb)

bisubstitutePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => UnifierBisubstitution ground
    -> Puzzle ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutePuzzle _ (ClosedExpression a) = return $ pure a
bisubstitutePuzzle bisub (OpenExpression constr puzzle) = do
    pe1 <- bisubstituteConstraint bisub constr
    per <- bisubstitutePuzzle bisub puzzle
    return $ per <*> pe1

bisubstitutesPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [UnifierBisubstitution ground]
    -> Puzzle ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesPuzzle bisubs = unEndoM $ mconcat $ fmap (MkEndoM . bisubstitutePuzzle) bisubs
