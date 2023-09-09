{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Solver.AtomicSubstitute
    ( SolverM
    , substituteAtomicPuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [SolverBisubstitution ground] (DolanTypeCheckM ground)

type Substitution :: GroundTypeKind -> Type
data Substitution ground where
    MkSubstitution
        :: forall (ground :: GroundTypeKind) polarity nv t.
           PolarityType polarity
        -> TypeVarT (JoinMeetType polarity nv t)
        -> TypeVarT nv
        -> UnifierM ground (DolanShimWit ground polarity (JoinMeetType polarity nv t))
        -> Maybe (DolanType ground (InvertPolarity polarity) t)
        -> Substitution ground

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Show (Substitution ground) where
    show (MkSubstitution pol oldvar newvar mt mi) = let
        invpol = invertPolarity pol
        st =
            case mToMaybe mt of
                Just (MkShimWit t _) -> withRepresentative pol $ showDolanType t
                Nothing -> "FAILS"
        si =
            case mi of
                Just invtype -> "; INV " <> withRepresentative invpol (showDolanType invtype)
                Nothing -> ""
        in "{" <>
           show oldvar <>
           show pol <> " => " <> st <> "; " <> show oldvar <> show invpol <> " => " <> show newvar <> si <> "}"

substBisubstitution ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => Substitution ground
    -> SolverBisubstitution ground
substBisubstitution (MkSubstitution (pol :: _ polarity) oldvar newvar mt _) =
    withRepresentative pol $
    withInvertPolarity @polarity $ let
        newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ invertPolarMap polar1
        in mkPolarBisubstitution oldvar mt $ return newVarWit

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

substituteAtomicChange ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => Substitution ground
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
substituteAtomicChange (MkSubstitution (pol :: _ polarity) oldvar newvar _ (Just t)) =
    invertSubstitution pol oldvar newvar t
substituteAtomicChange sub = bisubSubstitution $ substBisubstitution sub

-- | For debugging.
genNewName :: Bool
genNewName = False

substituteAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => AtomicConstraint ground a
    -> SolverM ground (a, Substitution ground)
substituteAtomicConstraint (MkAtomicConstraint oldvar (pol :: _ polarity) (fptw :: _ pt) recv) =
    withRepresentative pol $ do
        MkSomeTypeVarT (newvar :: TypeVarT newtv) <-
            if genNewName
                then lift renamerGenerateFreeUVar
                else return $ MkSomeTypeVarT oldvar
        withInvertPolarity @polarity $
            assignTypeVarT @(JoinMeetType polarity newtv pt) oldvar $ do
                let
                    newVarWit :: DolanShimWit ground (InvertPolarity polarity) (JoinMeetType polarity newtv pt)
                    newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ invertPolarMap polar1
                substwit <-
                    if recv
                        then do
                            MkSomeTypeVarT recvar <- lift renamerGenerateFreeUVar
                            assignSameTypeVarT oldvar recvar $
                                return $ \ptw' ->
                                    shimWitToDolan $
                                    recursiveDolanShimWit recvar $
                                    joinMeetShimWit
                                        (varDolanShimWit newvar)
                                        (bothBisubstitute oldvar (varDolanShimWit recvar) newVarWit ptw')
                        else return $ \ptw' -> joinMeetShimWit (varDolanShimWit newvar) ptw'
                subst <-
                    case fptw of
                        NormalFlipType ptw ->
                            return $ MkSubstitution pol oldvar newvar (return $ substwit $ mkPolarShimWit ptw) Nothing
                        InvertFlipType ptw ->
                            case isInvertInvertPolarity @polarity of
                                Refl -> do
                                    rigidity <- lift renamerGetNameRigidity
                                    return $
                                        MkSubstitution
                                            pol
                                            oldvar
                                            newvar
                                            (do
                                                 ptw' <- invertTypeM rigidity ptw
                                                 return $ substwit ptw')
                                            (Just ptw)
                tell [substBisubstitution subst]
                return (unPolarMap $ polar2 @(DolanShim ground) @polarity @newtv @pt, subst)

applySubstToAtomicPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => Substitution ground
    -> AtomicPuzzle ground a
    -> UnifierM ground (Puzzle ground a)
applySubstToAtomicPuzzle subst puzzle = mapExpressionWitnessesM (substituteAtomicChange subst) puzzle

substituteAtomicPiece ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => AtomicConstraint ground a
    -> AtomicPuzzle ground (a -> b)
    -> SolverM ground (Puzzle ground b)
substituteAtomicPiece piece puzzlerest = do
    (a, subst) <- substituteAtomicConstraint piece
    puzzlerest' <- lift $ lift $ runUnifierM $ applySubstToAtomicPuzzle subst puzzlerest
    return $ fmap (\ab -> ab a) puzzlerest'

substituteAtomicPuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => AtomicPuzzle ground a
    -> SolverM ground (Puzzle ground a)
substituteAtomicPuzzle (ClosedExpression a) = return $ pure a
substituteAtomicPuzzle (OpenExpression piece puzzlerest) = substituteAtomicPiece piece puzzlerest
