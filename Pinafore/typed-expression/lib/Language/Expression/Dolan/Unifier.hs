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
import Language.Expression.Dolan.Unifier.Piece
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.Solver
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Variance
import Shapes

{-
type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (UnifierConstraint ground)

type DolanUnifierExpression :: GroundTypeKind -> Type -> Type
type DolanUnifierExpression ground = DolanSolverExpression ground (UnifierConstraint ground)

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) tv t. IsDolanSubtypeGroundType ground
    => TypeVarT tv
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanShim ground t tv)
bisubstitutePositiveVar _ NilDolanType = pure initf
bisubstitutePositiveVar vn (ConsDolanType t1 tr) =
    OpenExpression (geSingleUnifierConstraint vn t1) $
    fmap (\fr f1 -> joinf (f1 . join1) fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) tv t. IsDolanSubtypeGroundType ground
    => TypeVarT tv
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanShim ground tv t)
bisubstituteNegativeVar _ NilDolanType = pure termf
bisubstituteNegativeVar vn (ConsDolanType t1 tr) =
    OpenExpression (leSingleUnifierConstraint vn t1) $
    fmap (\fr f1 -> meetf (meet1 . f1) fr) $ bisubstituteNegativeVar vn tr

bindUnifierMWit ::
       forall (ground :: GroundTypeKind) polarity wit t r. (IsDolanSubtypeGroundType ground)
    => UnifierM ground (DolanShimWit ground polarity t)
    -> (forall t'. DolanType ground polarity t' -> PolarMapType (DolanShim ground) polarity t t' -> Solver ground wit r)
    -> Solver ground wit r
bindUnifierMWit mst call = wbindUnifierM mst $ \(MkShimWit wt (MkPolarMap conv)) -> call wt conv

bisubstituteUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> DolanUnifier ground a
    -> UnifierSolver ground a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution _ vsub _ mwq) (OpenExpression (MkUnifierConstraint vwit PositiveType (NormalFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwq $ \tq convq ->
            bindUnifierMWit (bisubstituteType bisub tw) $ \tw' convw -> do
                conv <- unifyTypes tw' tq
                val' <- bisubstituteUnifier bisub expr
                pure $ val' $ convq . conv . convw
bisubstituteUnifier bisub@(MkBisubstitution _ vsub mwp _) (OpenExpression (MkUnifierConstraint vwit NegativeType (NormalFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwp $ \tp convp ->
            bindUnifierMWit (bisubstituteType bisub tw) $ \tw' convw -> do
                conv <- unifyTypes tp tw'
                val' <- bisubstituteUnifier bisub expr
                pure $ val' $ convw . conv . convp
bisubstituteUnifier bisub@(MkBisubstitution _ vsub mwp _) (OpenExpression (MkUnifierConstraint vwit NegativeType (InvertFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwp $ \tp convp -> do
            conv <- unifyTypes tp tw
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ conv . convp
bisubstituteUnifier bisub@(MkBisubstitution _ vsub _ mwq) (OpenExpression (MkUnifierConstraint vwit PositiveType (InvertFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwq $ \tq convq -> do
            conv <- unifyTypes tw tq
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ convq . conv
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vwit PositiveType (NormalFlipType tw) _) expr)
    | Just (MkShimWit (VarDolanSingularType v) (MkPolarMap conv)) <- dolanToMaybeTypeShim tw
    , Just Refl <- testEquality v vwit = do
        val' <- bisubstituteUnifier bisub expr
        pure $ val' conv
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vwit NegativeType (NormalFlipType tw) _) expr)
    | Just (MkShimWit (VarDolanSingularType v) (MkPolarMap conv)) <- dolanToMaybeTypeShim tw
    , Just Refl <- testEquality v vwit = do
        val' <- bisubstituteUnifier bisub expr
        pure $ val' conv
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vn PositiveType (NormalFlipType tw) _) expr) =
    bindUnifierMWit (bisubstituteType bisub tw) $ \tp' conv -> do
        val' <- bisubstituteUnifier bisub expr
        pv <- solverLiftTypeExpression $ bisubstitutePositiveVar vn tp'
        pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vn NegativeType (NormalFlipType tw) _) expr) =
    bindUnifierMWit (bisubstituteType bisub tw) $ \tp' conv -> do
        val' <- bisubstituteUnifier bisub expr
        pv <- solverLiftTypeExpression $ bisubstituteNegativeVar vn tp'
        pure $ val' $ conv . pv
bisubstituteUnifier bisub (OpenExpression subwit expr) = solverOpenExpression subwit $ bisubstituteUnifier bisub expr

type InvertSubstitution :: GroundTypeKind -> Type
data InvertSubstitution ground where
    MkInvertSubstitution
        :: forall (ground :: GroundTypeKind) polarity tv tv' t. (JoinMeetType (InvertPolarity polarity) tv' t ~ tv)
        => TypeVarT tv
        -> PolarityType polarity
        -> TypeVarT tv'
        -> DolanType ground polarity t
        -> InvertSubstitution ground

invertSubstitute ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => InvertSubstitution ground
    -> DolanUnifier ground a
    -> UnifierSolver ground a
invertSubstitute _ (ClosedExpression a) = pure a
invertSubstitute sub@(MkInvertSubstitution oldvar substpol newvar st) (OpenExpression (MkUnifierConstraint depvar unipol fvt recv) expr)
    | Just Refl <- testEquality oldvar depvar = let
        solv' = invertSubstitute sub expr
        in solverOpenExpression (MkUnifierConstraint newvar unipol fvt recv) $
           case (substpol, unipol) of
               (PositiveType, PositiveType) -> do
                   fa <- solv'
                   convm <-
                       case fvt of
                           NormalFlipType vt -> unifyTypes vt st
                           InvertFlipType vt -> unifyTypes vt st
                   pure $ \conv -> fa $ meetf conv convm
               (NegativeType, NegativeType) -> do
                   fa <- solv'
                   convm <-
                       case fvt of
                           NormalFlipType vt -> unifyTypes st vt
                           InvertFlipType vt -> unifyTypes st vt
                   pure $ \conv -> fa $ joinf conv convm
               (PositiveType, NegativeType) -> do
                   fa <- solv'
                   pure $ \conv -> fa $ conv . meet1
               (NegativeType, PositiveType) -> do
                   fa <- solv'
                   pure $ \conv -> fa $ join1 . conv
invertSubstitute sub (OpenExpression subwit expr) = solverOpenExpression subwit $ invertSubstitute sub expr
-}
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
    usubSubsumer [] subsumer = return $ solverExpressionLiftType subsumer
    usubSubsumer (s:ss) subsumer = do
        subsumer' <- lift $ runUnifierM $ bisubstitutePuzzle s subsumer
        usubSubsumerExpression @(DolanTypeSystem ground) ss $ solverExpressionLiftType subsumer'
    solveSubsumer = solvePuzzle
    subsumerPosSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumerNegSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = return $ puzzleExpressionUnify tinf tdecl

-- used for simplification, where all vars are fixed
checkSameVar ::
       forall (ground :: GroundTypeKind) t. IsDolanSubtypeGroundType ground
    => PuzzlePiece ground t
    -> DolanTypeCheckM ground t
{-
checkSameVar (MkUnifierConstraint va polwit (NormalFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) _)
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iJoinL1
            NegativeType -> iMeetR1
checkSameVar (MkUnifierConstraint va polwit (InvertFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) _)
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
    -> PuzzleExpression ground (DolanPolarMap ground polarity a b)
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
    puzzle <- evalPuzzleExpression $ puzzleSubsumeSingular ta tb
    solveExpression checkSameVar puzzle

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
type UnifierSolver ground = Solver ground (UnifierConstraint ground)

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
unifierSubtypeConversionAsGeneralAs _ _ = return undefined
    --- = subtypeConversionAsGeneralAs runCheckUnifier unifySubtypeContext'
