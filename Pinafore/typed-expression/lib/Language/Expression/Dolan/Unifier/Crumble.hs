{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Unifier.Crumble
    ( SolverM
    , solvePiece
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Piece
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Unroll
import Shapes

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground)

type Crumbler :: GroundTypeKind -> Type -> Type
newtype Crumbler ground a =
    MkCrumbler (SolverM ground (PuzzleExpression ground a))

instance forall (ground :: GroundTypeKind). Functor (DolanM ground) => Functor (Crumbler ground) where
    fmap ab (MkCrumbler mpa) = MkCrumbler $ (fmap $ fmap ab) mpa

instance forall (ground :: GroundTypeKind). Monad (DolanM ground) => Applicative (Crumbler ground) where
    pure a = MkCrumbler $ pure $ pure a
    MkCrumbler mpab <*> MkCrumbler mpa = MkCrumbler $ liftA2 (<*>) mpab mpa

instance forall (ground :: GroundTypeKind). MonadPlus (DolanM ground) => Alternative (Crumbler ground) where
    empty = MkCrumbler empty
    MkCrumbler p <|> MkCrumbler q = MkCrumbler $ p <|> q

instance forall (ground :: GroundTypeKind). Monad (DolanM ground) => WrappedApplicative (Crumbler ground) where
    type WAInnerM (Crumbler ground) = DolanTypeCheckM ground
    wexec msa =
        MkCrumbler $ do
            MkCrumbler sa <- lift msa
            sa
    whoist mm (MkCrumbler sb) = MkCrumbler $ hoist mm sb

runCrumbler ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Crumbler ground a
    -> DolanTypeCheckM ground (PuzzleExpression ground a, [UnifierBisubstitution ground])
runCrumbler (MkCrumbler wma) = runWriterT wma

crumblerLiftM ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => SolverM ground a
    -> Crumbler ground a
crumblerLiftM ma = MkCrumbler $ fmap pure ma

crumblerLiftPuzzleExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => PuzzleExpression ground a
    -> Crumbler ground a
crumblerLiftPuzzleExpression pexpr = MkCrumbler $ pure pexpr

crumblerLiftPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> Crumbler ground a
crumblerLiftPuzzle puzzle = crumblerLiftPuzzleExpression $ solverExpressionLiftType puzzle

crumblerLiftExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanOpenExpression ground a
    -> Crumbler ground a
crumblerLiftExpression expr = crumblerLiftPuzzleExpression $ solverExpressionLiftValue expr

crumbleReduced ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> Crumbler ground (DolanShim ground a b)
crumbleReduced ta tb = crumblerLiftPuzzle $ puzzleUnify ta tb

crumbleReducedWit ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanIsoShimWit ground pola a
    -> DolanIsoShimWit ground polb b
    -> Crumbler ground (DolanShim ground a b)
crumbleReducedWit (MkShimWit ta iconva) (MkShimWit tb iconvb) = let
    conva = polarPolyIsoPositive iconva
    convb = polarPolyIsoNegative iconvb
    in fmap (\conv -> convb . conv . conva) $ crumbleReduced ta tb

-- | For debugging.
genNewName :: Bool
genNewName = False

dpolar1 ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => PolarMap (DolanShim ground) (InvertPolarity polarity) (JoinMeetType polarity a b) a
dpolar1 =
    case polarityType @polarity of
        PositiveType -> MkPolarMap join1
        NegativeType -> MkPolarMap meet1

dpolar2 ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => PolarMapType (DolanShim ground) polarity b (JoinMeetType polarity a b)
dpolar2 =
    case polarityType @polarity of
        PositiveType -> join2
        NegativeType -> meet2

occursInFlipType ::
       forall (ground :: GroundTypeKind) polarity tv a. IsDolanGroundType ground
    => TypeVarT tv
    -> FlipType ground polarity a
    -> Bool
occursInFlipType var (NormalFlipType t) = occursInType var t
occursInFlipType var (InvertFlipType t) = occursInType var t

crumbleAtomic ::
       forall (ground :: GroundTypeKind) polarity a pt. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT a
    -> FlipType ground polarity pt
    -> Crumbler ground (PolarMapType (DolanShim ground) polarity pt a)
crumbleAtomic oldvar fptw =
    crumblerLiftM $ do
        MkSomeTypeVarT (newvar :: TypeVarT newtv) <-
            if genNewName
                then lift renamerGenerateFreeUVar
                else return $ MkSomeTypeVarT oldvar
        withInvertPolarity @polarity $
            assignTypeVarT @(JoinMeetType polarity newtv pt) oldvar $ do
                let
                    newVarWit :: DolanShimWit ground (InvertPolarity polarity) (JoinMeetType polarity newtv pt)
                    newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ dpolar1 @ground @polarity
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
            -- solver <-
                case fptw of
                    NormalFlipType ptw -> do
                        let
                            bisub =
                                mkPolarBisubstitution
                                    False
                                    oldvar
                                    (return $ substwit $ mkPolarShimWit ptw)
                                    (return newVarWit)
                        tell [bisub]
                        -- return $ bisubstituteUnifier bisub expr
                    InvertFlipType ptw ->
                        case isInvertInvertPolarity @polarity of
                            Refl -> do
                                rigidity <- lift renamerGetNameRigidity
                                let
                                    bisub =
                                        mkPolarBisubstitution
                                            False
                                            oldvar
                                            (return newVarWit)
                                            (do
                                                 ptw' <- invertTypeM rigidity ptw
                                                 return $ substwit ptw')
                                tell [bisub]
                                -- return $ invertSubstitute (MkInvertSubstitution oldvar (invertPolarity pol) newvar ptw) expr
            -- expr' <- lift $ runSolver solver
            -- expr'' <- runUnifierExpression expr'
            -- return $ fmap (\sa -> sa (dpolar2 @ground @polarity @newtv @pt pol)) $ expr''
                return $ dpolar2 @ground @polarity @newtv @pt

crumbleAtomicLE ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT a
    -> DolanType ground polarity b
    -> Crumbler ground (DolanShim ground a b)
crumbleAtomicLE v t =
    case polarityType @polarity of
        PositiveType -> crumbleAtomic v $ InvertFlipType t
        NegativeType -> crumbleAtomic v $ NormalFlipType t

crumbleAtomicGE ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT b
    -> DolanType ground polarity a
    -> Crumbler ground (DolanShim ground a b)
crumbleAtomicGE v t =
    case polarityType @polarity of
        PositiveType -> crumbleAtomic v $ NormalFlipType t
        NegativeType -> crumbleAtomic v $ InvertFlipType t

crumbleSubtypeContext ::
       forall (ground :: GroundTypeKind). (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => SubtypeContext (DolanVarID ground) (DolanType ground) (DolanShim ground) (Crumbler ground)
crumbleSubtypeContext = MkSubtypeContext crumbleReduced crumblerLiftExpression

crumbleGroundedTypes ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanGroundedType ground pola a
    -> DolanGroundedType ground polb b
    -> Crumbler ground (DolanPolyShim ground Type a b)
crumbleGroundedTypes = subtypeGroundedTypes crumbleSubtypeContext

fromJoinMeetLimit ::
       forall (shim :: ShimKind Type) polarity t. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => shim (JoinMeetType polarity t (LimitType polarity)) t
fromJoinMeetLimit =
    case polarityType @polarity of
        PositiveType -> iJoinL1
        NegativeType -> iMeetL1

toJoinMeetLimit ::
       forall (shim :: ShimKind Type) polarity t. (Is PolarityType polarity, JoinMeetIsoCategory shim)
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
    -> Crumbler ground (DolanShim ground a b)
crumbleSS (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
crumbleSS (VarDolanSingularType na) tb
    | isFreeVar na = fmap (\conv -> fromJoinMeetLimit @_ @polb . conv) $ crumbleAtomicLE na (singleDolanType tb)
crumbleSS ta (VarDolanSingularType nb)
    | isFreeVar nb = fmap (\conv -> conv . toJoinMeetLimit @_ @pola) $ crumbleAtomicGE nb (singleDolanType ta)
crumbleSS (GroundedDolanSingularType gta) (GroundedDolanSingularType gtb) = crumbleGroundedTypes gta gtb
crumbleSS (RecursiveDolanSingularType va pta) stb = crumbleReducedWit (unrollRecursiveType va pta) (typeToDolan stb)
crumbleSS sta (RecursiveDolanSingularType vb ptb) = crumbleReducedWit (typeToDolan sta) (unrollRecursiveType vb ptb)
crumbleSS _ _ = empty

crumbleSTN ::
       forall (ground :: GroundTypeKind) pola a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground pola a
    -> DolanType ground 'Negative b
    -> Crumbler ground (DolanShim ground a b)
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
    -> Crumbler ground (DolanShim ground a b)
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
    -> Crumbler ground (DolanShim ground a b)
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
    -> Crumbler ground (DolanShim ground a b)
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
    -> Crumbler ground (DolanShim ground a b)
crumbleTNS1 NilDolanType _ = empty
crumbleTNS1 (ConsDolanType t1 t2) tb =
    (fmap (\conv -> conv . meet1) $ crumbleSS t1 tb) <|> (fmap (\conv -> conv . meet2) $ crumbleTNS1 t2 tb)

crumbleTNS ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanSingularType ground polb b
    -> Crumbler ground (DolanShim ground a b)
crumbleTNS ta (VarDolanSingularType nb)
    | isFreeVar nb = crumbleAtomicGE nb ta
crumbleTNS ta (RecursiveDolanSingularType vb ptb) = crumbleReducedWit (typeToDolan ta) (unrollRecursiveType vb ptb)
crumbleTNS ta tb = crumbleTNS1 ta tb

crumbleTPT ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Positive a
    -> DolanType ground polb b
    -> Crumbler ground (DolanShim ground a b)
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
    -> Crumbler ground (DolanShim ground a b)
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
    -> Crumbler ground (DolanShim ground a b)
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
    -> Crumbler ground (DolanShim ground a b)
crumbleTT =
    case (polarityType @pola, polarityType @polb) of
        (PositiveType, _) -> crumbleTPT
        (NegativeType, NegativeType) -> crumbleTNTN
        (NegativeType, PositiveType) -> crumbleTNT

crumblePiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => PuzzlePiece ground a
    -> Crumbler ground a
crumblePiece (MkPuzzlePiece (NormalFlipType ta) (NormalFlipType tb)) = crumbleTT ta tb
crumblePiece (MkPuzzlePiece (NormalFlipType ta) (InvertFlipType tb)) = crumbleTT ta tb
crumblePiece (MkPuzzlePiece (InvertFlipType ta) (NormalFlipType tb)) = crumbleTT ta tb
crumblePiece (MkPuzzlePiece (InvertFlipType ta) (InvertFlipType tb)) = crumbleTT ta tb

solvePiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => PuzzlePiece ground a
    -> DolanTypeCheckM ground (PuzzleExpression ground a, [UnifierBisubstitution ground])
solvePiece piece = runCrumbler $ crumblePiece piece
