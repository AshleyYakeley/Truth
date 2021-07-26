{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Unifier.Build
    ( UnifierSolver
    , unifyTypes
    , subsumeSingularTypes
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Constraint
import Language.Expression.Dolan.Unroll
import Shapes

type UnifierSolver :: GroundTypeKind -> Type -> Type
type UnifierSolver ground = Solver ground (UnifierConstraint ground)

type UnificationSolver :: GroundTypeKind -> Type -> Type -> Type
type UnificationSolver ground a b = UnifierSolver ground (DolanShim ground a b)

unifySubtypeContext ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => SubtypeContext (DolanType ground) (DolanShim ground) (UnifierSolver ground)
unifySubtypeContext = MkSubtypeContext unifyTypes

unifyGroundTypes ::
       forall (ground :: GroundTypeKind) pola polb dva gta ta dvb gtb tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => ground dva gta
    -> DolanArguments dva (DolanType ground) gta pola ta
    -> ground dvb gtb
    -> DolanArguments dvb (DolanType ground) gtb polb tb
    -> UnificationSolver ground ta tb
unifyGroundTypes gta argsa gtb argsb = subtypeGroundTypes unifySubtypeContext gta argsa gtb argsb

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

isFreeVar :: (?rigidity :: String -> NameRigidity) => SymbolType var -> Bool
isFreeVar n =
    case ?rigidity $ witnessToValue n of
        FreeName -> True
        RigidName -> False

unifyTypesSS ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> UnificationSolver ground a b
unifyTypesSS (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifyTypesSS (VarDolanSingularType na) tb
    | isFreeVar na =
        fmap (\conv -> fromJoinMeetLimit @_ @polb . conv) $
        solverLiftExpression $ varExpression $ leSingleUnifierConstraint na tb
unifyTypesSS ta (VarDolanSingularType nb)
    | isFreeVar nb =
        fmap (\conv -> conv . toJoinMeetLimit @_ @pola) $
        solverLiftExpression $ varExpression $ geSingleUnifierConstraint nb ta
unifyTypesSS (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) =
    unifyGroundTypes gta argsa gtb argsb
unifyTypesSS sta@(RecursiveDolanSingularType _ _) stb = solveRecursiveSingularTypes unifyTypesTT sta stb
unifyTypesSS sta stb@(RecursiveDolanSingularType _ _) = solveRecursiveSingularTypes unifyTypesTT sta stb
unifyTypesSS _ _ = empty

unifyTypesSTN ::
       forall (ground :: GroundTypeKind) pola a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground pola a
    -> DolanType ground 'Negative b
    -> UnificationSolver ground a b
unifyTypesSTN _ NilDolanType = pure termf
unifyTypesSTN ta (ConsDolanType t1 t2) = do
    f1 <- unifyTypesSS ta t1
    f2 <- unifyTypesSTN ta t2
    return $ meetf f1 f2

unifyTypesSTP ::
       forall (ground :: GroundTypeKind) pola a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, ?rigidity :: String -> NameRigidity)
    => DolanSingularType ground pola a
    -> DolanType ground 'Positive b
    -> UnificationSolver ground a b
unifyTypesSTP _ NilDolanType = empty
unifyTypesSTP ta (ConsDolanType t1 t2) =
    (fmap (\conv -> join1 . conv) $ unifyTypesSS ta t1) <|> (fmap (\conv -> join2 . conv) $ unifyTypesSTP ta t2)

unifyTypesST1 ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesST1 =
    case polarityType @polb of
        NegativeType -> unifyTypesSTN
        PositiveType -> unifyTypesSTP

unifyTypesST ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanSingularType ground pola a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesST (VarDolanSingularType na) tb
    | isFreeVar na
    , PositiveType <- polarityType @polb = solverLiftExpression $ varExpression $ leUnifierConstraint na tb
unifyTypesST ta@(RecursiveDolanSingularType _ _) tb =
    unifyRecursiveType (singularRecursiveOrPlainType ta) (mkPolarShimWit $ PlainType tb)
unifyTypesST ta tb = unifyTypesST1 ta tb

unifyTypesTNS1 ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanSingularType ground polb b
    -> UnificationSolver ground a b
unifyTypesTNS1 NilDolanType _ = empty
unifyTypesTNS1 (ConsDolanType t1 t2) tb =
    (fmap (\conv -> conv . meet1) $ unifyTypesSS t1 tb) <|> (fmap (\conv -> conv . meet2) $ unifyTypesTNS1 t2 tb)

unifyTypesTNS ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanSingularType ground polb b
    -> UnificationSolver ground a b
unifyTypesTNS ta (VarDolanSingularType nb)
    | isFreeVar nb = solverLiftExpression $ varExpression $ geUnifierConstraint nb ta
unifyTypesTNS ta tb@(RecursiveDolanSingularType _ _) =
    unifyRecursiveType (mkPolarShimWit $ PlainType ta) (singularRecursiveOrPlainType tb)
unifyTypesTNS ta tb = unifyTypesTNS1 ta tb

unifyTypesTPT ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Positive a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesTPT NilDolanType _ = pure initf
unifyTypesTPT (ConsDolanType ta1 tar) tb = do
    f1 <- unifyTypesST ta1 tb
    f2 <- unifyTypesTPT tar tb
    return $ joinf f1 f2

unifyTypesTNT ::
       forall (ground :: GroundTypeKind) polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType polb, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesTNT NilDolanType NilDolanType =
    case polarityType @polb of
        PositiveType -> empty
        NegativeType -> pure id
unifyTypesTNT NilDolanType _ = empty
unifyTypesTNT (ConsDolanType ta1 tar) tb =
    (fmap (\conv -> conv . meet1) $ unifyTypesST ta1 tb) <|> (fmap (\conv -> conv . meet2) $ unifyTypesTNT tar tb)

unifyTypesTNTN ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => DolanType ground 'Negative a
    -> DolanType ground 'Negative b
    -> UnificationSolver ground a b
unifyTypesTNTN _ NilDolanType = pure termf
unifyTypesTNTN ta (ConsDolanType t1 t2) = do
    f1 <- unifyTypesTNS ta t1
    f2 <- unifyTypesTNTN ta t2
    return $ meetf f1 f2

unifyTypesTT ::
       forall (ground :: GroundTypeKind) pola polb a b.
       ( IsDolanSubtypeGroundType ground
       , Is PolarityType pola
       , Is PolarityType polb
       , ?rigidity :: String -> NameRigidity
       )
    => DolanType ground pola a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesTT ta tb =
    (case (polarityType @pola, polarityType @polb) of
         (PositiveType, _) -> unifyTypesTPT
         (NegativeType, NegativeType) -> unifyTypesTNTN
         (NegativeType, PositiveType) -> unifyTypesTNT)
        ta
        tb

unifySingularTypes ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> UnificationSolver ground a b
unifySingularTypes ta tb =
    wbind renamerGetNameRigidity $ \rigidity -> let
        ?rigidity = rigidity
        in unifyTypesSS ta tb

unifyTypes ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypes ta tb =
    wremonad (remonad $ tackOnTypeConvertError ta tb) $
    wbind renamerGetNameRigidity $ \rigidity -> let
        ?rigidity = rigidity
        in unifyTypesTT ta tb

unifyRecursiveType ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) pola a
    -> PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) polb b
    -> UnificationSolver ground a b
unifyRecursiveType ta tb = solveRecursiveShimWits unifyTypes ta tb

subsumeSingularTypes ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> UnifierSolver ground (DolanPolarMap ground polarity a b)
subsumeSingularTypes ta tb =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ unifySingularTypes ta tb
        NegativeType -> fmap MkPolarMap $ unifySingularTypes tb ta
