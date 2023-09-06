module Language.Expression.Dolan.Unifier.AtomicSubstitute
    ( SolverM
    , substituteAtomicPuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.AtomicConstraint
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.Substitution
import Language.Expression.Dolan.Unifier.UnifierM
import Shapes

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground)

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
