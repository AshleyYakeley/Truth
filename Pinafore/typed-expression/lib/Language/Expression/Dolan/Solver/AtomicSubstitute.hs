{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Solver.AtomicSubstitute
    ( getAtomicConstraint
    , SolverM
    , solveAtomicConstraint
    , substituteAtomicConstraint
    , bisubstitutesAtomicConstraint
    , bisubstitutesPuzzle
    , applySubstsToPuzzle
    , substBisubstitution
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.FlipType
import Language.Expression.Dolan.Invert
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [Substitution ground] (CrumbleM ground)

substBisubstitution ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => Substitution ground
    -> SolverBisubstitution ground
substBisubstitution (MkSubstitution (pol :: _ polarity) oldvar newvar mt _) =
    withRepresentative pol $
    withInvertPolarity @polarity $ let
        newVarWit =
            shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ invertPolarShim $ isoRetractPolyPolar1 @ground
        in mkPolarBisubstitution oldvar mt $ return newVarWit

substituteAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => Substitution ground
    -> AtomicConstraint ground a
    -> TypeResult ground (Puzzle ground a)
substituteAtomicConstraint (MkSubstitution substpol oldvar newvar _ (Just st)) (MkAtomicConstraint depvar unipol fvt)
    | Just Refl <- testEquality oldvar depvar =
        return $ let
            p1 = atomicConstraintPuzzle (MkAtomicConstraint newvar unipol fvt)
            p2 =
                case (substpol, unipol) of
                    (NegativeType, PositiveType) -> do
                        convm <- flipToType fvt $ \vt -> puzzleUnify vt st
                        pure $ \conv -> meetf conv convm
                    (PositiveType, NegativeType) -> do
                        convm <- flipToType fvt $ \vt -> puzzleUnify st vt
                        pure $ \conv -> joinf conv convm
                    (NegativeType, NegativeType) -> pure $ \conv -> conv . meet1
                    (PositiveType, PositiveType) -> pure $ \conv -> join1 . conv
            in liftA2 (\t a -> a t) p1 p2
substituteAtomicConstraint sub ac = bisubstituteAtomicConstraint (substBisubstitution sub) ac

isoRetractPolyPolarShim ::
       forall (pshim :: PolyShimKind) polarity a b. (FunctionShim (pshim Type), Is PolarityType polarity)
    => PolarShim (pshim Type) polarity a b
    -> PolarShim (PolyIso pshim Type) polarity a b
isoRetractPolyPolarShim (MkPolarShim conv) =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ MkPolyMapT $ isoRetractShim conv
        NegativeType -> MkPolarShim $ MkPolyMapT $ isoRetractShim conv

isoRetractPolyPolar1 ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanGroundType ground, Is PolarityType polarity)
    => PolarShim (DolanPolyIsoShim ground Type) polarity a (JoinMeetType polarity a b)
isoRetractPolyPolar1 =
    case polarityType @polarity of
        PositiveType -> MkPolarShim $ MkPolyMapT isoRetractJoin1
        NegativeType -> MkPolarShim $ MkPolyMapT isoRetractMeet1

-- | For debugging.
genNewName :: Bool
genNewName = False

getAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => AtomicConstraint ground a
    -> DolanTypeCheckM ground (a, Substitution ground)
getAtomicConstraint (MkAtomicConstraint oldvar (pol :: _ polarity) (fptw :: _ pt)) =
    withRepresentative pol $ do
        MkSomeTypeVarT (newvar :: TypeVarT newtv) <-
            if genNewName
                then renamerGenerateFreeUVar
                else return $ MkSomeTypeVarT oldvar
        withInvertPolarity @polarity $
            assignTypeVarT @(JoinMeetType polarity newtv pt) oldvar $ do
                let
                    newVarWit :: DolanIsoShimWit ground (InvertPolarity polarity) (JoinMeetType polarity newtv pt)
                    newVarWit =
                        shimWitToDolan $
                        MkShimWit (VarDolanSingularType newvar) $ invertPolarShim $ isoRetractPolyPolar1 @ground
                substwit <-
                    if occursInFlipType oldvar fptw
                        then do
                            MkSomeTypeVarT recvar <- renamerGenerateFreeUVar
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
                                    rigidity <- renamerGetNameRigidity
                                    return $
                                        MkSubstitution
                                            pol
                                            oldvar
                                            newvar
                                            (do
                                                 MkShimWit pt conv <- invertTypeM rigidity ptw
                                                 return $ substwit $ MkShimWit pt $ isoRetractPolyPolarShim conv)
                                            (Just ptw)
                return (unPolarShim $ polar2 @(DolanShim ground) @polarity @newtv @pt, subst)

solveAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => AtomicConstraint ground a
    -> SolverM ground a
solveAtomicConstraint ac = do
    (a, subst) <- lift $ liftToCrumbleM $ getAtomicConstraint ac
    tell [subst]
    return a

bisubstitutesPiece ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [SolverBisubstitution ground]
    -> Piece ground a
    -> TypeResult ground (Puzzle ground a)
bisubstitutesPiece newchanges (WholePiece wc) = do
    MkShimWit wc' conv <- bisubstitutesWholeConstraintShim newchanges $ mkShimWit wc
    return $ fmap (isoBackwards conv) $ varExpression $ WholePiece wc'
bisubstitutesPiece newchanges (AtomicPiece ac) = bisubstitutesAtomicConstraint newchanges ac

bisubstitutesPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [SolverBisubstitution ground]
    -> Puzzle ground a
    -> TypeResult ground (Puzzle ground a)
bisubstitutesPuzzle substs = mapExpressionWitnessesM $ bisubstitutesPiece substs

bisubstitutesAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [SolverBisubstitution ground]
    -> AtomicConstraint ground a
    -> TypeResult ground (Puzzle ground a)
bisubstitutesAtomicConstraint [] ac = return $ atomicConstraintPuzzle ac
bisubstitutesAtomicConstraint (s:ss) ac = do
    puzzle <- bisubstituteAtomicConstraint s ac
    bisubstitutesPuzzle ss puzzle

applySubstsToPiece ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [Substitution ground]
    -> Piece ground a
    -> TypeResult ground (Puzzle ground a)
applySubstsToPiece newchanges = bisubstitutesPiece (fmap substBisubstitution newchanges)

applySubstsToPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [Substitution ground]
    -> Puzzle ground a
    -> TypeResult ground (Puzzle ground a)
applySubstsToPuzzle substs = mapExpressionWitnessesM $ applySubstsToPiece substs
