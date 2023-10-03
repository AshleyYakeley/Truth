{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Solver.Crumble.Type
    ( crumbleConstraint
    , checkCrumbleArguments
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.FlipType
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

newtype TypeCrumbler (ground :: GroundTypeKind) a = MkTypeCrumbler
    { unTypeCrumbler :: ReaderT (TypeError ground) (CrumbleM ground) (NonEmpty (PuzzleExpression ground a))
    }

concatResults :: Result err (NonEmpty a) -> Result err (NonEmpty a) -> Result err (NonEmpty a)
concatResults (FailureResult _) r = r
concatResults r (FailureResult _) = r
concatResults (SuccessResult a) (SuccessResult b) = SuccessResult $ a <> b

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground, Functor (DolanM ground)) =>
             Functor (TypeCrumbler ground) where
    fmap ab (MkTypeCrumbler mpa) = MkTypeCrumbler $ (fmap $ fmap $ fmap ab) mpa

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground, Monad (DolanM ground)) =>
             Applicative (TypeCrumbler ground) where
    pure a = MkTypeCrumbler $ pure $ pure $ pure a
    MkTypeCrumbler mpab <*> MkTypeCrumbler mpa = MkTypeCrumbler $ liftA2 (liftA2 (<*>)) mpab mpa

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground, Monad (DolanM ground)) =>
             Alternative (TypeCrumbler ground) where
    empty = MkTypeCrumbler $ ReaderT throw
    MkTypeCrumbler (ReaderT p) <|> MkTypeCrumbler (ReaderT q) =
        MkTypeCrumbler $
        ReaderT $ \r -> liftFullToCrumbleMWithUnlift $ \unlift -> liftA2 concatResults (unlift $ p r) (unlift $ q r)

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground, Monad (DolanM ground)) =>
             WrappedApplicative (TypeCrumbler ground) where
    type WAInnerM (TypeCrumbler ground) = ReaderT (TypeError ground) (CrumbleM ground)
    wexec msa =
        MkTypeCrumbler $ do
            MkTypeCrumbler sa <- msa
            sa
    whoist mm (MkTypeCrumbler sb) = MkTypeCrumbler $ mm sb

crumblerLiftPuzzleExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => PuzzleExpression ground a
    -> TypeCrumbler ground a
crumblerLiftPuzzleExpression pexpr = MkTypeCrumbler $ pure $ pure pexpr

crumblerLiftPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> TypeCrumbler ground a
crumblerLiftPuzzle puzzle = crumblerLiftPuzzleExpression $ puzzleExpression puzzle

crumblerLiftExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanOpenExpression ground a
    -> TypeCrumbler ground a
crumblerLiftExpression expr = crumblerLiftPuzzleExpression $ solverExpressionLiftValue expr

crumbleReduced ::
       forall (ground :: GroundTypeKind) pola polb ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola ta
    -> DolanType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleReduced ta tb = crumblerLiftPuzzle $ puzzleUnify ta tb

crumbleReducedWit ::
       forall (ground :: GroundTypeKind) pola polb ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanIsoShimWit ground pola ta
    -> DolanIsoShimWit ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleReducedWit (MkShimWit ta iconva) (MkShimWit tb iconvb) = let
    conva = polarPolyIsoPositive iconva
    convb = polarPolyIsoNegative iconvb
    in fmap (\conv -> convb . conv . conva) $ crumbleReduced ta tb

crumbleAtomic ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => AtomicConstraint ground a
    -> TypeCrumbler ground a
crumbleAtomic ac = crumblerLiftPuzzle $ atomicConstraintPuzzle ac

crumbleAtomicLE ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT ta
    -> DolanType ground polarity tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleAtomicLE v t = crumbleAtomic $ leAtomicConstraint v t

crumbleAtomicGE ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tb
    -> DolanType ground polarity ta
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleAtomicGE v t = crumbleAtomic $ geAtomicConstraint v t

crumbleArgument ::
       forall (ground :: GroundTypeKind) pola polb sv ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => CCRPolarArgument (DolanType ground) pola sv ta
    -> CCRPolarArgument (DolanType ground) polb sv tb
    -> TypeCrumbler ground (CCRVarianceCategory (DolanShim ground) sv ta tb)
crumbleArgument (CoCCRPolarArgument ta) (CoCCRPolarArgument tb) = crumbleTT ta tb
crumbleArgument (ContraCCRPolarArgument ta) (ContraCCRPolarArgument tb) =
    withInvertPolarity @pola $
    withInvertPolarity @polb $ do
        ba <- crumbleTT tb ta
        return $ MkCatDual ba
crumbleArgument (RangeCCRPolarArgument tpa tqa) (RangeCCRPolarArgument tpb tqb) =
    withInvertPolarity @pola $
    withInvertPolarity @polb $ do
        pba <- crumbleTT tpb tpa
        qab <- crumbleTT tqa tqb
        return $ MkCatRange pba qab

crumbleArguments ::
       forall (ground :: GroundTypeKind) pola polb dv (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => CCRVariancesType dv
    -> CCRVariancesMap dv gta
    -> CCRVariancesMap dv gtb
    -> CCRPolarArguments dv (DolanType ground) gta pola ta
    -> CCRPolarArguments dv (DolanType ground) gtb polb tb
    -> TypeCrumbler ground (DolanPolyShim ground (CCRVariancesKind dv) gta gtb -> DolanShim ground ta tb)
crumbleArguments NilListType NilCCRVariancesMap NilCCRVariancesMap NilCCRArguments NilCCRArguments = pure id
crumbleArguments (ConsListType svt dvt) (ConsCCRVariancesMap ccrva dvma) (ConsCCRVariancesMap ccrvb dvmb) (ConsCCRArguments sta dta) (ConsCCRArguments stb dtb) =
    case ccrVarianceCoercibleKind svt of
        Dict -> do
            sfunc <- crumbleArgument @ground @pola @polb sta stb
            f <- crumbleArguments dvt dvma dvmb dta dtb
            pure $ \conv -> f (applyPolyShim svt ccrva ccrvb conv sfunc)

crumbleDolanArguments ::
       forall (ground :: GroundTypeKind) pola polb dv gt ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => CCRVariancesMap dv gt
    -> CCRPolarArguments dv (DolanType ground) gt pola ta
    -> CCRPolarArguments dv (DolanType ground) gt polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleDolanArguments dvm argsa argsb = let
    dvt = ccrArgumentsType argsa
    in case ccrVariancesCategory @(DolanPolyShim ground) dvt of
           Dict -> fmap (\f -> f id) $ crumbleArguments dvt dvm dvm argsa argsb

crumbleSubtypeChain ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb pola polb ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => SubtypeChain ground dva gta dvb gtb
    -> CCRVariancesMap dvb gtb
    -> CCRPolarArguments dva (DolanType ground) gta pola ta
    -> CCRPolarArguments dvb (DolanType ground) gtb polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleSubtypeChain NilSubtypeChain dvm args args' = crumbleDolanArguments dvm args args'
crumbleSubtypeChain (ConsSubtypeChain (MkSubtypeLink dvmb argsb argsc expr) chain) dvmc argsa argsc' =
    (\convb convab conva -> convb . convab . conva) <$> (crumbleDolanArguments dvmc argsc argsc') <*>
    (crumblerLiftExpression expr) <*>
    (crumbleSubtypeChain chain dvmb argsa argsb)

crumbleGroundedTypes ::
       forall (ground :: GroundTypeKind) pola polb ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanGroundedType ground pola ta
    -> DolanGroundedType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleGroundedTypes (MkDolanGroundedType ga argsa) (MkDolanGroundedType gb argsb) =
    wbind (lift $ getSubtypeChainRenamed ga gb) $ \chain ->
        crumbleSubtypeChain chain (groundTypeVarianceMap gb) argsa argsb

fromJoinMeetLimit ::
       forall (shim :: ShimKind Type) polarity t. (Is PolarityType polarity, JoinMeetIsoShim shim)
    => shim (JoinMeetType polarity t (LimitType polarity)) t
fromJoinMeetLimit =
    case polarityType @polarity of
        PositiveType -> iJoinL1
        NegativeType -> iMeetL1

toJoinMeetLimit ::
       forall (shim :: ShimKind Type) polarity t. (Is PolarityType polarity, JoinMeetIsoShim shim)
    => shim t (JoinMeetType polarity t (LimitType polarity))
toJoinMeetLimit =
    case polarityType @polarity of
        PositiveType -> iJoinR1
        NegativeType -> iMeetR1

isFreeVar :: (?rigidity :: String -> NameRigidity) => TypeVarT tv -> Bool
isFreeVar n =
    case ?rigidity $ typeVarName n of
        FreeName -> True
        RigidName -> False

crumbleSS ::
       forall (ground :: GroundTypeKind) pola polb ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola ta
    -> DolanSingularType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleSS (RecursiveDolanSingularType va pta) stb = crumbleReducedWit (unrollRecursiveType va pta) (typeToDolan stb)
crumbleSS sta (RecursiveDolanSingularType vb ptb) = crumbleReducedWit (typeToDolan sta) (unrollRecursiveType vb ptb)
crumbleSS (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
crumbleSS (VarDolanSingularType na) tb
    | isFreeVar na = fmap (\conv -> fromJoinMeetLimit @_ @polb . conv) $ crumbleAtomicLE na (singleDolanType tb)
crumbleSS ta (VarDolanSingularType nb)
    | isFreeVar nb = fmap (\conv -> conv . toJoinMeetLimit @_ @pola) $ crumbleAtomicGE nb (singleDolanType ta)
crumbleSS (GroundedDolanSingularType gta) (GroundedDolanSingularType gtb) = crumbleGroundedTypes gta gtb
crumbleSS _ _ = empty

crumbleSTN ::
       forall (ground :: GroundTypeKind) pola ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground pola ta
    -> DolanType ground 'Negative tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleSTN _ NilDolanType = pure termf
crumbleSTN ta (ConsDolanType t1 t2) = do
    f1 <- crumbleSS ta t1
    f2 <- crumbleSTN ta t2
    return $ meetf f1 f2

crumbleSTP ::
       forall (ground :: GroundTypeKind) pola ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground pola ta
    -> DolanType ground 'Positive tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleSTP _ NilDolanType = empty
crumbleSTP ta (ConsDolanType t1 t2) =
    (fmap (\conv -> join1 . conv) $ crumbleSS ta t1) <|> (fmap (\conv -> join2 . conv) $ crumbleSTP ta t2)

crumbleST1 ::
       forall (ground :: GroundTypeKind) pola polb ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola ta
    -> DolanType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleST1 =
    case polarityType @polb of
        NegativeType -> crumbleSTN
        PositiveType -> crumbleSTP

crumbleST ::
       forall (ground :: GroundTypeKind) pola polb ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola ta
    -> DolanType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleST (VarDolanSingularType na) tb
    | isFreeVar na
    , PositiveType <- polarityType @polb = crumbleAtomicLE na tb
crumbleST (RecursiveDolanSingularType va pta) tb = crumbleReducedWit (unrollRecursiveType va pta) (typeToDolan tb)
crumbleST ta tb = crumbleST1 ta tb

crumbleTNS1 ::
       forall (ground :: GroundTypeKind) polb ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative ta
    -> DolanSingularType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleTNS1 NilDolanType _ = empty
crumbleTNS1 (ConsDolanType t1 t2) tb =
    (fmap (\conv -> conv . meet1) $ crumbleSS t1 tb) <|> (fmap (\conv -> conv . meet2) $ crumbleTNS1 t2 tb)

crumbleTNS ::
       forall (ground :: GroundTypeKind) polb ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative ta
    -> DolanSingularType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleTNS ta (VarDolanSingularType nb)
    | isFreeVar nb = crumbleAtomicGE nb ta
crumbleTNS ta (RecursiveDolanSingularType vb ptb) = crumbleReducedWit (typeToDolan ta) (unrollRecursiveType vb ptb)
crumbleTNS ta tb = crumbleTNS1 ta tb

crumbleTPT ::
       forall (ground :: GroundTypeKind) polb ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Positive ta
    -> DolanType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleTPT NilDolanType _ = pure initf
crumbleTPT (ConsDolanType ta1 tar) tb = do
    f1 <- crumbleST ta1 tb
    f2 <- crumbleTPT tar tb
    return $ joinf f1 f2

crumbleTNT ::
       forall (ground :: GroundTypeKind) polb ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative ta
    -> DolanType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleTNT NilDolanType NilDolanType =
    case polarityType @polb of
        PositiveType -> empty
        NegativeType -> pure id
crumbleTNT NilDolanType _ = empty
crumbleTNT (ConsDolanType ta1 tar) tb =
    (fmap (\conv -> conv . meet1) $ crumbleST ta1 tb) <|> (fmap (\conv -> conv . meet2) $ crumbleTNT tar tb)

crumbleTNTN ::
       forall (ground :: GroundTypeKind) ta tb. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative ta
    -> DolanType ground 'Negative tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleTNTN _ NilDolanType = pure termf
crumbleTNTN ta (ConsDolanType t1 t2) = do
    f1 <- crumbleTNS ta t1
    f2 <- crumbleTNTN ta t2
    return $ meetf f1 f2

crumbleTT ::
       forall (ground :: GroundTypeKind) pola polb ta tb.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanType ground pola ta
    -> DolanType ground polb tb
    -> TypeCrumbler ground (DolanShim ground ta tb)
crumbleTT ta tb =
    case (polarityType @pola, polarityType @polb) of
        (PositiveType, _) -> crumbleTPT ta tb
        (NegativeType, NegativeType) -> crumbleTNTN ta tb
        (NegativeType, PositiveType) -> crumbleTNT ta tb

crumbleWholeConstraint ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => WholeConstraint ground a
    -> TypeCrumbler ground a
crumbleWholeConstraint (MkWholeConstraint (NormalFlipType ta) (NormalFlipType tb)) = crumbleTT ta tb
crumbleWholeConstraint (MkWholeConstraint (NormalFlipType ta) (InvertFlipType tb)) = crumbleTT ta tb
crumbleWholeConstraint (MkWholeConstraint (InvertFlipType ta) (NormalFlipType tb)) = crumbleTT ta tb
crumbleWholeConstraint (MkWholeConstraint (InvertFlipType ta) (InvertFlipType tb)) = crumbleTT ta tb

crumbleConstraint ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground)
    => WholeConstraint ground a
    -> CrumbleM ground (NonEmpty (PuzzleExpression ground a))
crumbleConstraint constr@(MkWholeConstraint fta ftb) = do
    rigidity <- crumbleMRigidity
    let
        ?rigidity = rigidity
        in runReaderT (unTypeCrumbler $ crumbleWholeConstraint constr) $ ConvertTypeError fta ftb

checkCrumbleArguments ::
       forall (ground :: GroundTypeKind) pola polb dv gt ta tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => CCRVariancesMap dv gt
    -> CCRPolarArguments dv (DolanType ground) gt pola ta
    -> CCRPolarArguments dv (DolanType ground) gt polb tb
    -> DolanTypeCheckM ground Bool
checkCrumbleArguments dvm argsa argsb = do
    rigidity <- renamerGetNameRigidity
    rpexprs <-
        runCrumbleMResult rigidity $ let
            ?rigidity = rigidity
            in runReaderT (unTypeCrumbler $ crumbleDolanArguments dvm argsa argsb) $ error "bad type crumble"
    return $
        case rpexprs of
            SuccessResult _ -> True
            FailureResult _ -> False
