{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Solver.AtomicSubstitute
    ( SolverM
    , substituteAtomicConstraint
    , substituteAtomicChange
    , bisubstitutesAtomicConstraint
    , bisubstitutesPuzzle
    , applySubstsToPuzzle
    , substBisubstitution
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [Substitution ground] (DolanTypeCheckM ground)

substBisubstitution ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => Substitution ground
    -> SolverBisubstitution ground
substBisubstitution (MkSubstitution (pol :: _ polarity) oldvar newvar mt _) =
    withRepresentative pol $
    withInvertPolarity @polarity $ let
        newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ invertPolarMap $ halfBrokenShim @ground
        in mkPolarBisubstitution oldvar mt $ return newVarWit

invertSubstitution ::
       forall (ground :: GroundTypeKind) polarity v t a. (IsDolanGroundType ground)
    => PolarityType polarity
    -> TypeVarT (JoinMeetType polarity v t)
    -> TypeVarT v
    -> DolanType ground (InvertPolarity polarity) t
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
invertSubstitution substpol oldvar newvar st (MkAtomicConstraint depvar unipol fvt)
    | Just Refl <- testEquality oldvar depvar =
        return $ let
            p1 = atomicConstraintPuzzle (MkAtomicConstraint newvar unipol fvt)
            p2 =
                case (substpol, unipol) of
                    (NegativeType, PositiveType) -> do
                        convm <-
                            case fvt of
                                NormalFlipType vt -> puzzleUnify False vt st
                                InvertFlipType vt -> puzzleUnify False vt st
                        pure $ \conv -> meetf conv convm
                    (PositiveType, NegativeType) -> do
                        convm <-
                            case fvt of
                                NormalFlipType vt -> puzzleUnify False st vt
                                InvertFlipType vt -> puzzleUnify False st vt
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
substituteAtomicChange sub = bisubstituteAtomicConstraint $ substBisubstitution sub

brokenShim ::
       forall (shim :: ShimKind Type) a b. FunctionShim shim
    => shim a b
brokenShim = functionToShim "BROKEN" $ \_ -> error "broken shim"

{-
brokenPolarShim ::
       forall (shim :: ShimKind Type) polarity a b. (FunctionShim shim, Is PolarityType polarity)
    => PolarMap shim polarity a b
brokenPolarShim =
    case polarityType @polarity of
        PositiveType -> MkPolarMap brokenShim
        NegativeType -> MkPolarMap brokenShim
-}
brokenToIsoShim ::
       forall (shim :: ShimKind Type) a b. FunctionShim shim
    => shim a b
    -> Isomorphism shim a b
brokenToIsoShim conv = MkIsomorphism conv brokenShim

brokenToPolyIsoShim ::
       forall (pshim :: PolyShimKind) a b. FunctionShim (pshim Type)
    => pshim Type a b
    -> PolyIso pshim Type a b
brokenToPolyIsoShim conv = MkPolyMapT $ brokenToIsoShim conv

brokenPolarToPolyIsoShim ::
       forall (pshim :: PolyShimKind) polarity a b. (FunctionShim (pshim Type), Is PolarityType polarity)
    => PolarMap (pshim Type) polarity a b
    -> PolarMap (PolyIso pshim Type) polarity a b
brokenPolarToPolyIsoShim (MkPolarMap conv) =
    case polarityType @polarity of
        PositiveType -> MkPolarMap $ brokenToPolyIsoShim conv
        NegativeType -> MkPolarMap $ brokenToPolyIsoShim conv

halfBrokenShim ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanGroundType ground, Is PolarityType polarity)
    => PolarMap (DolanPolyIsoShim ground Type) polarity a (JoinMeetType polarity a b)
halfBrokenShim =
    case polarityType @polarity of
        PositiveType -> MkPolarMap $ MkPolyMapT $ MkIsomorphism join1 $ joinf id brokenShim
        NegativeType -> MkPolarMap $ MkPolyMapT $ MkIsomorphism meet1 $ meetf id brokenShim

-- | For debugging.
genNewName :: Bool
genNewName = False

substituteAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => AtomicConstraint ground a
    -> SolverM ground a
substituteAtomicConstraint (MkAtomicConstraint oldvar (pol :: _ polarity) (fptw :: _ pt)) =
    withRepresentative pol $ do
        MkSomeTypeVarT (newvar :: TypeVarT newtv) <-
            if genNewName
                then lift renamerGenerateFreeUVar
                else return $ MkSomeTypeVarT oldvar
        withInvertPolarity @polarity $
            assignTypeVarT @(JoinMeetType polarity newtv pt) oldvar $ do
                let
                    newVarWit :: DolanIsoShimWit ground (InvertPolarity polarity) (JoinMeetType polarity newtv pt)
                    newVarWit =
                        shimWitToDolan $
                        MkShimWit (VarDolanSingularType newvar) $ invertPolarMap $ halfBrokenShim @ground
                substwit <-
                    if occursInFlipType oldvar fptw
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
                                                 MkShimWit pt conv <- invertTypeM rigidity ptw
                                                 return $ substwit $ MkShimWit pt $ brokenPolarToPolyIsoShim conv)
                                            (Just ptw)
                tell [subst]
                return $ unPolarMap $ polar2 @(DolanShim ground) @polarity @newtv @pt

bisubstitutesPiece ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [SolverBisubstitution ground]
    -> Piece ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesPiece newchanges (WholePiece wc memo) = do
    MkShimWit wc' conv <- bisubstitutesWholeConstraintShim newchanges $ mkShimWit wc
    return $ fmap (isoBackwards conv) $ varExpression $ WholePiece wc' memo
bisubstitutesPiece newchanges (AtomicPiece ac) = bisubstitutesAtomicConstraint newchanges ac

bisubstitutesPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [SolverBisubstitution ground]
    -> Puzzle ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesPuzzle substs = mapExpressionWitnessesM $ bisubstitutesPiece substs

bisubstitutesAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [SolverBisubstitution ground]
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesAtomicConstraint [] ac = return $ atomicConstraintPuzzle ac
bisubstitutesAtomicConstraint (s:ss) ac = do
    puzzle <- bisubstituteAtomicConstraint s ac
    bisubstitutesPuzzle ss puzzle

applySubstsToPiece ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [Substitution ground]
    -> Piece ground a
    -> UnifierM ground (Puzzle ground a)
applySubstsToPiece newchanges = bisubstitutesPiece (fmap substBisubstitution newchanges)

applySubstsToPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [Substitution ground]
    -> Puzzle ground a
    -> UnifierM ground (Puzzle ground a)
applySubstsToPuzzle substs = mapExpressionWitnessesM $ applySubstsToPiece substs
