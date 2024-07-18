module Language.Expression.Dolan.Simplify.AutomateRecursion
    ( automateRecursion
    , automateRecursionInType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unroll
import Shapes

class HasRecursion t where
    hasRecursion :: t -> Bool

instance (forall polarity' t'. HasRecursion (ft polarity' t')) => HasRecursion (CCRPolarArgument ft polarity sv t) where
    hasRecursion (CoCCRPolarArgument t) = hasRecursion t
    hasRecursion (ContraCCRPolarArgument t) = hasRecursion t
    hasRecursion (RangeCCRPolarArgument p q) = hasRecursion p || hasRecursion q

instance forall (w :: CCRArgumentKind) dv gt t. (forall sv a. HasRecursion (w sv a)) =>
             HasRecursion (CCRArguments w dv gt t) where
    hasRecursion NilCCRArguments = False
    hasRecursion (ConsCCRArguments arg1 argr) = hasRecursion arg1 || hasRecursion argr

instance forall (ground :: GroundTypeKind) polarity t. HasRecursion (DolanGroundedType ground polarity t) where
    hasRecursion (MkDolanGroundedType _ args) = hasRecursion args

instance forall (ground :: GroundTypeKind) polarity t. HasRecursion (DolanSingularType ground polarity t) where
    hasRecursion (GroundedDolanSingularType t) = hasRecursion t
    hasRecursion (VarDolanSingularType _) = False
    hasRecursion (RecursiveDolanSingularType _ _) = True

instance forall (ground :: GroundTypeKind) polarity t. HasRecursion (DolanType ground polarity t) where
    hasRecursion NilDolanType = False
    hasRecursion (ConsDolanType t1 tr) = hasRecursion t1 || hasRecursion tr

data Equation (ground :: GroundTypeKind) where
    MkEquation
        :: forall (ground :: GroundTypeKind) polarity t.
           PolarityType polarity
        -> TypeVarT t
        -> DolanType ground polarity t
        -> Equation ground

instance forall (ground :: GroundTypeKind). (ShowGroundType ground, IsDolanGroundType ground) => Show (Equation ground) where
    show (MkEquation pol var t) = withRepresentative pol $ show var <> " = " <> allShow t <> " " <> show pol

lookupEquation ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [Equation ground]
    -> TypeVarT t
    -> Maybe (DolanType ground polarity t)
lookupEquation [] _ = Nothing
lookupEquation (MkEquation pol var t:_) var'
    | Just Refl <- testEquality pol (polarityType @polarity)
    , Just Refl <- testEquality var var' = Just t
lookupEquation (_:ee) var = lookupEquation ee var

type Extracter (ground :: GroundTypeKind) = Writer [Equation ground]

class ExtractEquations (ground :: GroundTypeKind) (t :: Type) where
    extractEquations :: t -> Extracter ground t

instance forall (ground :: GroundTypeKind) ft polarity sv t. ( Is PolarityType polarity
         , forall polarity' t'. Is PolarityType polarity' => ExtractEquations ground (ft polarity' t')
         ) => ExtractEquations ground (CCRPolarArgument ft polarity sv t) where
    extractEquations (CoCCRPolarArgument t) = fmap CoCCRPolarArgument $ extractEquations t
    extractEquations (ContraCCRPolarArgument t) =
        withInvertPolarity @polarity $ fmap ContraCCRPolarArgument $ extractEquations t
    extractEquations (RangeCCRPolarArgument p q) =
        withInvertPolarity @polarity $ liftA2 RangeCCRPolarArgument (extractEquations p) (extractEquations q)

instance forall (ground :: GroundTypeKind) (w :: CCRArgumentKind) dv gt t. (forall sv a.
                                                                                    ExtractEquations ground (w sv a)) =>
             ExtractEquations ground (CCRArguments w dv gt t) where
    extractEquations NilCCRArguments = return NilCCRArguments
    extractEquations (ConsCCRArguments arg1 argr) =
        liftA2 ConsCCRArguments (extractEquations arg1) (extractEquations argr)

instance forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity =>
             ExtractEquations ground (DolanGroundedType ground polarity t) where
    extractEquations (MkDolanGroundedType g args) = fmap (MkDolanGroundedType g) $ extractEquations args

instance forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity =>
             ExtractEquations ground (DolanSingularType ground polarity t) where
    extractEquations (GroundedDolanSingularType t) = fmap GroundedDolanSingularType $ extractEquations t
    extractEquations t@(VarDolanSingularType _) = return t
    extractEquations (RecursiveDolanSingularType var t) = do
        t' <- extractEquations t
        tell $ pure $ MkEquation (polarityType @polarity) var t'
        return $ VarDolanSingularType var

instance forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity =>
             ExtractEquations ground (DolanType ground polarity t) where
    extractEquations NilDolanType = pure NilDolanType
    extractEquations (ConsDolanType t1 tr) = liftA2 ConsDolanType (extractEquations t1) (extractEquations tr)

lookupMemo ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [Equation ground]
    -> DolanType ground polarity t
    -> Maybe (TypeVarT t)
lookupMemo [] _ = Nothing
lookupMemo (MkEquation pol var t:_) t'
    | Just Refl <- testEquality pol (polarityType @polarity)
    , Just Refl <- testEquality t t' = Just var
lookupMemo (_:ee) t = lookupMemo ee t

substEqnsSingular ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [Equation ground]
    -> DolanSingularType ground polarity t
    -> DolanShimWit ground polarity t
substEqnsSingular eqns (VarDolanSingularType var)
    | Just t <- lookupEquation eqns var = substEqns eqns t
substEqnsSingular _ (RecursiveDolanSingularType var t) = switchShimWit polarPolyIsoForwards $ unrollRecursiveType var t
substEqnsSingular _ st = typeToDolan st

substEqns ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => [Equation ground]
    -> DolanType ground polarity t
    -> DolanShimWit ground polarity t
substEqns _ NilDolanType = nilDolanShimWit
substEqns eqns (ConsDolanType t1 tr) = joinMeetShimWit (substEqnsSingular eqns t1) (substEqns eqns tr)

mergeGroundeds ::
       forall (ground :: GroundTypeKind) polarity ta tb. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity ta
    -> DolanSingularType ground polarity tb
    -> Maybe (DolanSingularShimWit ground polarity (JoinMeetType polarity ta tb))
mergeGroundeds (GroundedDolanSingularType (MkDolanGroundedType gt1 args1)) (GroundedDolanSingularType (MkDolanGroundedType gt2 args2))
    | Just (Refl, HRefl) <- groundTypeTestEquality gt1 gt2 =
        Just $
        case mergeDolanArguments joinMeetType (groundTypeVarianceMap gt1) args1 args2 of
            MkShimWit args' convargs -> MkShimWit (GroundedDolanSingularType (MkDolanGroundedType gt1 args')) convargs
mergeGroundeds (VarDolanSingularType v1) (VarDolanSingularType v2)
    | Just Refl <- testEquality v1 v2 = Just $ MkShimWit (VarDolanSingularType v1) $ polarF id id
mergeGroundeds _ _ = Nothing

mergeGroundMaybe ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanType ground polarity tr
    -> Maybe (DolanShimWit ground polarity (JoinMeetType polarity t1 tr))
mergeGroundMaybe _ NilDolanType = Nothing
mergeGroundMaybe ts (ConsDolanType t1 tr) =
    case mergeGroundeds ts t1 of
        Nothing ->
            case mergeGroundMaybe ts tr of
                Just (MkShimWit tsr conv) ->
                    Just $
                    MkShimWit (ConsDolanType t1 tsr) $ polarF (polar2 . conv . polar1) (iPolarPair id $ conv . polar2)
                Nothing -> Nothing
        Just (MkShimWit ts1 conv1) -> Just $ mapPolarShimWit (iPolarPair conv1 id . iPolarSwapR) $ mergeGround ts1 tr

mergeGround ::
       forall (ground :: GroundTypeKind) polarity t1 tr. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t1
    -> DolanType ground polarity tr
    -> DolanShimWit ground polarity (JoinMeetType polarity t1 tr)
mergeGround st t =
    case mergeGroundMaybe st t of
        Just t' -> t'
        Nothing -> mkShimWit $ ConsDolanType st t

mergeGrounds ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanShimWit ground polarity t
mergeGrounds NilDolanType = nilDolanShimWit
mergeGrounds (ConsDolanType t1 tr) =
    case mergeGrounds tr of
        MkShimWit tr' conv -> mapShimWit (iPolarPair id conv) $ mergeGround t1 tr'

automatonGroundedType ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => Is PolarityType polarity =>
               [Equation ground] -> [Equation ground] -> DolanGroundedType ground polarity t -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
automatonGroundedType eqns memos gt = fmap shimWitToDolan $ mapDolanGroundedTypeM (automatonType eqns memos) gt

automatonSingularType ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => Is PolarityType polarity =>
               [Equation ground] -> [Equation ground] -> DolanSingularType ground polarity t -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
automatonSingularType eqns memos (GroundedDolanSingularType gt) = automatonGroundedType eqns memos gt
automatonSingularType _ _ t = return $ typeToDolan t

automatonEachSingularType ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => Is PolarityType polarity =>
               [Equation ground] -> [Equation ground] -> DolanType ground polarity t -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
automatonEachSingularType _ _ NilDolanType = return nilDolanShimWit
automatonEachSingularType eqns memos (ConsDolanType t1 tr) = do
    t1' <- automatonSingularType eqns memos t1
    tr' <- automatonEachSingularType eqns memos tr
    return $ joinMeetShimWit t1' tr'

automatonType ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => Is PolarityType polarity =>
               [Equation ground] -> [Equation ground] -> DolanType ground polarity t -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
automatonType _ memos t0
    | Just var <- lookupMemo memos t0 = return $ varDolanShimWit var
automatonType eqns memos t0 = do
    MkShimWit t1 conv1 <- return $ mergeGrounds t0
    let tw2 = substEqns eqns t1
    MkShimWit t3 conv3 <- return $ chainShimWit mergeGrounds tw2
    case lookupMemo memos t3 of
        Just var -> return $ mapShimWit (conv3 . conv1) $ varDolanShimWit var
        Nothing -> do
            var <- renamerGenerateAssign
            let
                memo :: Equation ground
                memo = MkEquation (polarityType @polarity) var t3
            tw4 <- automatonEachSingularType eqns (memo : memos) t3
            return $
                mapShimWit (conv3 . conv1) $
                if variableOccursIn var tw4
                    then shimWitToDolan $ recursiveDolanShimWit var tw4
                    else tw4

automateRecursionInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanTypeCheckM ground (DolanShimWit ground polarity t)
automateRecursionInType t
    | hasRecursion t = do
        t' <- unEndoM (varRename fixedRenameSource) t
        let (t0, eqns) = runWriter $ extractEquations @ground t'
        automatonType eqns [] t0
automateRecursionInType t = return $ mkShimWit t

automateRecursion ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => EndoM (DolanTypeCheckM ground) a
automateRecursion = mapPShimWitsM automateRecursionInType automateRecursionInType
