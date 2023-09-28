{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Solver.Crumble.Type
    ( crumbleConstraint
    , makeSCAGA
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

type TypeCrumbler :: GroundTypeKind -> Type -> Type
newtype TypeCrumbler ground a = MkTypeCrumbler
    { runTypeCrumbler :: DolanTypeCheckM ground (PuzzleExpression ground a)
    }

instance forall (ground :: GroundTypeKind). Functor (DolanM ground) => Functor (TypeCrumbler ground) where
    fmap ab (MkTypeCrumbler mpa) = MkTypeCrumbler $ (fmap $ fmap ab) mpa

instance forall (ground :: GroundTypeKind). Monad (DolanM ground) => Applicative (TypeCrumbler ground) where
    pure a = MkTypeCrumbler $ pure $ pure a
    MkTypeCrumbler mpab <*> MkTypeCrumbler mpa = MkTypeCrumbler $ liftA2 (<*>) mpab mpa

instance forall (ground :: GroundTypeKind). MonadPlus (DolanM ground) => Alternative (TypeCrumbler ground) where
    empty = MkTypeCrumbler empty
    MkTypeCrumbler p <|> MkTypeCrumbler q = MkTypeCrumbler $ p <|> q

instance forall (ground :: GroundTypeKind). Monad (DolanM ground) => WrappedApplicative (TypeCrumbler ground) where
    type WAInnerM (TypeCrumbler ground) = DolanTypeCheckM ground
    wexec msa =
        MkTypeCrumbler $ do
            MkTypeCrumbler sa <- msa
            sa
    whoist mm (MkTypeCrumbler sb) = MkTypeCrumbler $ mm sb

crumblerLiftPuzzleExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => PuzzleExpression ground a
    -> TypeCrumbler ground a
crumblerLiftPuzzleExpression pexpr = MkTypeCrumbler $ pure pexpr

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
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleReduced ta tb = crumblerLiftPuzzle $ puzzleUnify ta tb

crumbleReducedWit ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanIsoShimWit ground pola a
    -> DolanIsoShimWit ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
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
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT a
    -> DolanType ground polarity b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleAtomicLE v t = crumbleAtomic $ leAtomicConstraint v t

crumbleAtomicGE ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT b
    -> DolanType ground polarity a
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleAtomicGE v t = crumbleAtomic $ geAtomicConstraint v t

crumbleSubtypeContext ::
       forall (ground :: GroundTypeKind). (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => SubtypeContext (DolanVarID ground) (DolanType ground) (DolanShim ground) (TypeCrumbler ground)
crumbleSubtypeContext = MkSubtypeContext crumbleTT crumblerLiftExpression

crumbleGroundedTypes ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanGroundedType ground pola a
    -> DolanGroundedType ground polb b
    -> TypeCrumbler ground (DolanPolyShim ground Type a b)
crumbleGroundedTypes = subtypeGroundedTypes crumbleSubtypeContext

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
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
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
       forall (ground :: GroundTypeKind) pola a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground pola a
    -> DolanType ground 'Negative b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleSTN _ NilDolanType = pure termf
crumbleSTN ta (ConsDolanType t1 t2) = do
    f1 <- crumbleSS ta t1
    f2 <- crumbleSTN ta t2
    return $ meetf f1 f2

crumbleSTP ::
       forall (ground :: GroundTypeKind) pola a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground pola a
    -> DolanType ground 'Positive b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleSTP _ NilDolanType = empty
crumbleSTP ta (ConsDolanType t1 t2) =
    (fmap (\conv -> join1 . conv) $ crumbleSS ta t1) <|> (fmap (\conv -> join2 . conv) $ crumbleSTP ta t2)

crumbleST1 ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola a
    -> DolanType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleST1 =
    case polarityType @polb of
        NegativeType -> crumbleSTN
        PositiveType -> crumbleSTP

crumbleST ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola a
    -> DolanType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleST (VarDolanSingularType na) tb
    | isFreeVar na
    , PositiveType <- polarityType @polb = crumbleAtomicLE na tb
crumbleST (RecursiveDolanSingularType va pta) tb = crumbleReducedWit (unrollRecursiveType va pta) (typeToDolan tb)
crumbleST ta tb = crumbleST1 ta tb

crumbleTNS1 ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanSingularType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleTNS1 NilDolanType _ = empty
crumbleTNS1 (ConsDolanType t1 t2) tb =
    (fmap (\conv -> conv . meet1) $ crumbleSS t1 tb) <|> (fmap (\conv -> conv . meet2) $ crumbleTNS1 t2 tb)

crumbleTNS ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanSingularType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleTNS ta (VarDolanSingularType nb)
    | isFreeVar nb = crumbleAtomicGE nb ta
crumbleTNS ta (RecursiveDolanSingularType vb ptb) = crumbleReducedWit (typeToDolan ta) (unrollRecursiveType vb ptb)
crumbleTNS ta tb = crumbleTNS1 ta tb

crumbleTPT ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Positive a
    -> DolanType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleTPT NilDolanType _ = pure initf
crumbleTPT (ConsDolanType ta1 tar) tb = do
    f1 <- crumbleST ta1 tb
    f2 <- crumbleTPT tar tb
    return $ joinf f1 f2

crumbleTNT ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleTNT NilDolanType NilDolanType =
    case polarityType @polb of
        PositiveType -> empty
        NegativeType -> pure id
crumbleTNT NilDolanType _ = empty
crumbleTNT (ConsDolanType ta1 tar) tb =
    (fmap (\conv -> conv . meet1) $ crumbleST ta1 tb) <|> (fmap (\conv -> conv . meet2) $ crumbleTNT tar tb)

crumbleTNTN ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanType ground 'Negative b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleTNTN _ NilDolanType = pure termf
crumbleTNTN ta (ConsDolanType t1 t2) = do
    f1 <- crumbleTNS ta t1
    f2 <- crumbleTNTN ta t2
    return $ meetf f1 f2

crumbleTT ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanType ground pola a
    -> DolanType ground polb b
    -> TypeCrumbler ground (DolanShim ground a b)
crumbleTT ta tb =
    whoist (hoist $ tackOnTypeConvertError ta tb) $
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
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => WholeConstraint ground a
    -> DolanTypeCheckM ground (PuzzleExpression ground a)
crumbleConstraint constr = runTypeCrumbler $ crumbleWholeConstraint constr

runCheckCrumble ::
       forall (ground :: GroundTypeKind) t. IsDolanSubtypeGroundType ground
    => (forall a. Puzzle ground a -> DolanTypeCheckM ground (DolanOpenExpression ground a))
    -> TypeCrumbler ground t
    -> DolanTypeCheckM ground Bool
runCheckCrumble solvePuzzle ca = do
    altIs $ do
        MkSolverExpression puzzle _ <- runTypeCrumbler ca
        _ <- solvePuzzle puzzle
        return ()

makeSCAGA ::
       forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
       IsDolanSubtypeGroundType ground
    => (forall a. Puzzle ground a -> DolanTypeCheckM ground (DolanOpenExpression ground a))
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> DolanM ground Bool
makeSCAGA solvePuzzle = let
    ?rigidity = \_ -> RigidName
    in subtypeConversionAsGeneralAs (runCheckCrumble solvePuzzle) crumbleSubtypeContext
