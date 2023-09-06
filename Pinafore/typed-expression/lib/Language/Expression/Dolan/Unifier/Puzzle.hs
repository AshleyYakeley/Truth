{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Unifier.Puzzle where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.AtomicConstraint
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.Substitution
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Unifier.WholeConstraint
import Shapes

substituteAtomicChange ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => Substitution ground
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
substituteAtomicChange (MkSubstitution (pol :: _ polarity) oldvar newvar _ (Just t)) =
    invertSubstitution pol oldvar newvar t
substituteAtomicChange sub = bisubSubstitution $ substBisubstitution sub

type Piece :: GroundTypeKind -> Type -> Type
data Piece ground t where
    WholePiece :: forall (ground :: GroundTypeKind) t. WholeConstraint ground t -> Piece ground t
    AtomicPiece :: forall (ground :: GroundTypeKind) t. AtomicConstraint ground t -> Piece ground t

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (Piece ground t) where
    show (WholePiece wc) = "whole: " <> show wc
    show (AtomicPiece ac) = "atomic: " <> show ac

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (Piece ground) where
    allConstraint = Dict

type Puzzle :: GroundTypeKind -> Type -> Type
type Puzzle ground = Expression (Piece ground)

wholeConstraintPuzzle :: forall (ground :: GroundTypeKind) a. WholeConstraint ground a -> Puzzle ground a
wholeConstraintPuzzle constr = varExpression $ WholePiece constr

atomicConstraintPuzzle :: forall (ground :: GroundTypeKind) a. AtomicConstraint ground a -> Puzzle ground a
atomicConstraintPuzzle ac = varExpression $ AtomicPiece ac

flipUnifyPuzzle ::
       forall (ground :: GroundTypeKind) a b.
       FlipType ground 'Positive a
    -> FlipType ground 'Negative b
    -> Puzzle ground (DolanShim ground a b)
flipUnifyPuzzle fta ftb = wholeConstraintPuzzle $ MkWholeConstraint fta ftb

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
    in flipUnifyPuzzle fta ftb

type PuzzleExpression :: GroundTypeKind -> Type -> Type
type PuzzleExpression ground = TSOpenSolverExpression (DolanTypeSystem ground) (Puzzle ground)

puzzleExpression ::
       forall (ground :: GroundTypeKind) a. ()
    => Puzzle ground a
    -> PuzzleExpression ground a
puzzleExpression = solverExpressionLiftType

puzzleExpressionUnify ::
       forall (ground :: GroundTypeKind) pola polb a b. (Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> PuzzleExpression ground (DolanShim ground a b)
puzzleExpressionUnify ta tb = puzzleExpression $ puzzleUnify ta tb

puzzleUnifySingular ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> Puzzle ground (DolanShim ground a b)
puzzleUnifySingular ta tb =
    fmap (\conv -> iJoinMeetL1 @_ @polb . conv . iJoinMeetR1 @_ @pola) $
    puzzleUnify (singleDolanType ta) (singleDolanType tb)

bisubSubstitution ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => UnifierBisubstitution ground
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
bisubSubstitution bisub@(MkBisubstitution oldvar _ mwq) (MkAtomicConstraint depvar PositiveType ftw _)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tq (MkPolarMap convq) <- mwq
        MkShimWit ftw' (MkPolarMap convw) <- bisubstituteFlipType bisub ftw
        return $ fmap (\conv -> convq . conv . convw) $ flipUnifyPuzzle ftw' (NormalFlipType tq)
bisubSubstitution bisub@(MkBisubstitution oldvar mwp _) (MkAtomicConstraint depvar NegativeType ftw _)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tp (MkPolarMap convp) <- mwp
        MkShimWit ftw' (MkPolarMap convw) <- bisubstituteFlipType bisub ftw
        return $ fmap (\conv -> convw . conv . convp) $ flipUnifyPuzzle (NormalFlipType tp) ftw'
bisubSubstitution _ ac
    | Just conv <- isPureAtomicConstraint ac = return $ pure conv
bisubSubstitution bisub (MkAtomicConstraint depvar PositiveType (NormalFlipType tw) _) = do
    MkShimWit tp (MkPolarMap conv) <- bisubstituteType bisub tw
    return $ fmap (\pv -> pv . conv) $ atomicConstraintPuzzle $ mkAtomicConstraint depvar $ NormalFlipType tp
bisubSubstitution bisub (MkAtomicConstraint depvar NegativeType (NormalFlipType tw) _) = do
    MkShimWit tp (MkPolarMap conv) <- bisubstituteType bisub tw
    return $ fmap (\pv -> conv . pv) $ atomicConstraintPuzzle $ mkAtomicConstraint depvar $ NormalFlipType tp
bisubSubstitution _ ac = return $ atomicConstraintPuzzle ac

bisubstitutesWholeConstraintPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [UnifierBisubstitution ground]
    -> WholeConstraint ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesWholeConstraintPuzzle bisubs wc = do
    MkShimWit wc' (MkCatDual conv) <- bisubstitutesWholeConstraintShim bisubs $ mkShimWit wc
    return $ fmap conv $ wholeConstraintPuzzle wc'

invertSubstitution ::
       forall (ground :: GroundTypeKind) polarity v t a. (IsDolanGroundType ground)
    => PolarityType polarity
    -> TypeVarT (JoinMeetType polarity v t)
    -> TypeVarT v
    -> DolanType ground (InvertPolarity polarity) t
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
invertSubstitution substpol oldvar newvar st (MkAtomicConstraint depvar unipol fvt recv)
    | Just Refl <- testEquality oldvar depvar =
        return $ let
            p1 = atomicConstraintPuzzle (MkAtomicConstraint newvar unipol fvt recv)
            p2 =
                case (substpol, unipol) of
                    (NegativeType, PositiveType) -> do
                        convm <-
                            case fvt of
                                NormalFlipType vt -> puzzleUnify vt st
                                InvertFlipType vt -> puzzleUnify vt st
                        pure $ \conv -> meetf conv convm
                    (PositiveType, NegativeType) -> do
                        convm <-
                            case fvt of
                                NormalFlipType vt -> puzzleUnify st vt
                                InvertFlipType vt -> puzzleUnify st vt
                        pure $ \conv -> joinf conv convm
                    (NegativeType, NegativeType) -> pure $ \conv -> conv . meet1
                    (PositiveType, PositiveType) -> pure $ \conv -> join1 . conv
            in liftA2 (\t a -> a t) p1 p2
invertSubstitution _ _ _ _ ac = return $ atomicConstraintPuzzle ac
