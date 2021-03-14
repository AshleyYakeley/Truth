{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Unifier
    ( UnifierBisubstitution
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.UnifierM
import Shapes

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground (DolanPolyShim ground Type) (UnifierM ground)

type BisubstitutionWitness :: GroundTypeKind -> Type -> Type
data BisubstitutionWitness ground t where
    -- | var <: type
    LEBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) polarity name t. Is PolarityType polarity
        => SymbolType name
        -> DolanType ground polarity t
        -> Bool
        -> BisubstitutionWitness ground (DolanPolyShim ground Type (UVarT name) t)
    -- | var :> type
    GEBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) polarity name t. Is PolarityType polarity
        => SymbolType name
        -> DolanType ground polarity t
        -> Bool
        -> BisubstitutionWitness ground (DolanPolyShim ground Type t (UVarT name))

instance forall (ground :: GroundTypeKind) t. (IsDolanGroundType ground) => Show (BisubstitutionWitness ground t) where
    show (LEBisubstitutionWitness var wt recflag) = let
        rs :: String
        rs =
            if recflag
                then "rec"
                else "nonrec"
        in show var <> " <: " <> showDolanType wt <> " (" <> rs <> ")"
    show (GEBisubstitutionWitness var wt recflag) = let
        rs :: String
        rs =
            if recflag
                then "rec"
                else "nonrec"
        in show var <> " :> " <> showDolanType wt <> " (" <> rs <> ")"

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground) =>
             AllWitnessConstraint Show (BisubstitutionWitness ground) where
    allWitnessConstraint = Dict

leBisubstitutionWitness ::
       forall (ground :: GroundTypeKind) polarity name p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanSingularType ground polarity p
    -> BisubstitutionWitness ground (DolanPolyShim ground Type (UVarT name) (JoinMeetType polarity p (LimitType polarity)))
leBisubstitutionWitness var spt = LEBisubstitutionWitness var (singleDolanType spt) (occursInSingularType var spt)

geBisubstitutionWitness ::
       forall (ground :: GroundTypeKind) polarity name p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanSingularType ground polarity p
    -> BisubstitutionWitness ground (DolanPolyShim ground Type (JoinMeetType polarity p (LimitType polarity)) (UVarT name))
geBisubstitutionWitness var spt = GEBisubstitutionWitness var (singleDolanType spt) (occursInSingularType var spt)

type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (BisubstitutionWitness ground)

type FullUnifier :: GroundTypeKind -> Type -> Type
type FullUnifier ground = Solver ground (BisubstitutionWitness ground)

type DolanUnification :: GroundTypeKind -> Polarity -> Polarity -> Type -> Type -> Type
type DolanUnification ground pola polb a b = FullUnifier ground (DolanPolyShim ground Type a b)

unifySubtypeContext ::
       forall (ground :: GroundTypeKind) pola polb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (FullUnifier ground) pola polb
unifySubtypeContext = let
    subtypeConvert = unifyTypes
    subtypeInverted ::
           SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (FullUnifier ground) (InvertPolarity polb) (InvertPolarity pola)
    subtypeInverted = invertPolarity @pola $ invertPolarity @polb $ unifySubtypeContext
    in MkSubtypeContext {..}

unifyGroundTypes ::
       forall (ground :: GroundTypeKind) pola polb dva gta ta dvb gtb tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => ground dva gta
    -> DolanArguments dva (DolanType ground) gta pola ta
    -> ground dvb gtb
    -> DolanArguments dvb (DolanType ground) gtb polb tb
    -> DolanUnification ground pola polb ta tb
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

unifySingularTypes ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> DolanUnification ground pola polb a b
unifySingularTypes (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifySingularTypes (VarDolanSingularType na) tb =
    wbind renamerGetIsNameRigid $ \isrigid ->
        if isrigid $ witnessToValue na
            then empty
            else fmap (\conv -> fromJoinMeetLimit @_ @polb . conv) $
                 solverLiftExpression $ varExpression $ leBisubstitutionWitness na tb
unifySingularTypes ta (VarDolanSingularType nb) =
    wbind renamerGetIsNameRigid $ \isrigid ->
        if isrigid $ witnessToValue nb
            then empty
            else fmap (\conv -> conv . toJoinMeetLimit @_ @pola) $
                 solverLiftExpression $ varExpression $ geBisubstitutionWitness nb ta
unifySingularTypes (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) =
    unifyGroundTypes gta argsa gtb argsb
unifySingularTypes sta@(RecursiveDolanSingularType _ _) stb = solveRecursiveSingularTypes unifyTypes sta stb
unifySingularTypes sta stb@(RecursiveDolanSingularType _ _) = solveRecursiveSingularTypes unifyTypes sta stb

unifyTypes1Neg ::
       forall (ground :: GroundTypeKind) pola a b. (IsDolanSubtypeGroundType ground, Is PolarityType pola)
    => DolanSingularType ground pola a
    -> DolanType ground 'Negative b
    -> DolanUnification ground pola 'Negative a b
unifyTypes1Neg _ NilDolanType = pure termf
unifyTypes1Neg ta (ConsDolanType t1 t2) = do
    f1 <- unifySingularTypes ta t1
    f2 <- unifyTypes1Neg ta t2
    return $ meetf f1 f2

unifyTypes1Pos ::
       forall (ground :: GroundTypeKind) pola a b. (IsDolanSubtypeGroundType ground, Is PolarityType pola)
    => DolanSingularType ground pola a
    -> DolanType ground 'Positive b
    -> DolanUnification ground pola 'Positive a b
unifyTypes1Pos _ NilDolanType = empty
unifyTypes1Pos ta (ConsDolanType t1 t2) =
    (fmap (\conv -> join1 . conv) $ unifySingularTypes ta t1) <|> (fmap (\conv -> join2 . conv) $ unifyTypes1Pos ta t2)

unifyTypes1 ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanType ground polb b
    -> DolanUnification ground pola polb a b
unifyTypes1 =
    case polarityType @polb of
        NegativeType -> unifyTypes1Neg
        PositiveType -> unifyTypes1Pos

unifyTypesPos ::
       forall (ground :: GroundTypeKind) polb a b. (IsDolanSubtypeGroundType ground, Is PolarityType polb)
    => DolanType ground 'Positive a
    -> DolanType ground polb b
    -> DolanUnification ground 'Positive polb a b
unifyTypesPos NilDolanType _ = pure initf
unifyTypesPos (ConsDolanType ta1 tar) tb = do
    f1 <- unifyTypes1 ta1 tb
    f2 <- unifyTypesPos tar tb
    return $ joinf f1 f2

unifyTypesNeg ::
       forall (ground :: GroundTypeKind) polb a b. (IsDolanSubtypeGroundType ground, Is PolarityType polb)
    => DolanType ground 'Negative a
    -> DolanType ground polb b
    -> DolanUnification ground 'Negative polb a b
unifyTypesNeg NilDolanType _ = empty
unifyTypesNeg (ConsDolanType ta1 tar) tb =
    (fmap (\conv -> conv . meet1) $ unifyTypes1 ta1 tb) <|> (fmap (\conv -> conv . meet2) $ unifyTypesNeg tar tb)

unifyTypes ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> DolanUnification ground pola polb a b
unifyTypes =
    case polarityType @pola of
        PositiveType -> unifyTypesPos
        NegativeType -> unifyTypesNeg

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanPolyShim ground Type t (UVarT name))
bisubstitutePositiveVar _ NilDolanType = pure initf
bisubstitutePositiveVar vn (ConsDolanType t1 tr) =
    OpenExpression (geBisubstitutionWitness vn t1) $
    fmap (\fr f1 -> joinf (f1 . join1) fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanPolyShim ground Type (UVarT name) t)
bisubstituteNegativeVar _ NilDolanType = pure termf
bisubstituteNegativeVar vn (ConsDolanType t1 tr) =
    OpenExpression (leBisubstitutionWitness vn t1) $
    fmap (\fr f1 -> meetf (meet1 . f1) fr) $ bisubstituteNegativeVar vn tr

bindUnifierMWit ::
       forall (ground :: GroundTypeKind) polarity wit t r. (IsDolanSubtypeGroundType ground)
    => UnifierM ground (DolanShimWit ground polarity t)
    -> (forall t'.
                DolanType ground polarity t' -> PolarMapType (DolanPolyShim ground Type) polarity t t' -> Solver ground wit r)
    -> Solver ground wit r
bindUnifierMWit mst call = wbind (lift $ runUnifierM mst) $ \(MkShimWit wt (MkPolarMap conv)) -> call wt conv

bisubstituteUnifier ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground)
    => UnifierBisubstitution ground
    -> DolanUnifier ground a
    -> FullUnifier ground a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution _ vsub mwp _) (OpenExpression (LEBisubstitutionWitness vwit (tw :: _ polarity _) _) uval)
    | Just Refl <- testEquality vsub vwit
    , PositiveType <- polarityType @polarity =
        bindUnifierMWit mwp $ \tp convp -> do
            conv <- unifyTypes tp tw
            val' <- bisubstituteUnifier bisub uval
            pure $ val' $ conv . convp
bisubstituteUnifier bisub@(MkBisubstitution _ vsub _ mwq) (OpenExpression (GEBisubstitutionWitness vwit (tw :: _ polarity _) _) uval)
    | Just Refl <- testEquality vsub vwit
    , NegativeType <- polarityType @polarity =
        bindUnifierMWit mwq $ \tq convq -> do
            conv <- unifyTypes tw tq
            val' <- bisubstituteUnifier bisub uval
            pure $ val' $ convq . conv
bisubstituteUnifier bisub (OpenExpression (LEBisubstitutionWitness vn (tw :: _ polarity _) _) uval)
    | NegativeType <- polarityType @polarity =
        bindUnifierMWit (bisubstituteShimWit bisub $ mkShimWit tw) $ \tp' conv -> do
            val' <- bisubstituteUnifier bisub uval
            pv <- solverLiftExpression $ bisubstituteNegativeVar vn tp'
            pure $ val' $ conv . pv
bisubstituteUnifier bisub (OpenExpression (GEBisubstitutionWitness vn (tw :: _ polarity _) _) uval)
    | PositiveType <- polarityType @polarity =
        bindUnifierMWit (bisubstituteShimWit bisub $ mkShimWit tw) $ \tp' conv -> do
            val' <- bisubstituteUnifier bisub uval
            pv <- solverLiftExpression $ bisubstitutePositiveVar vn tp'
            pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression subwit uval) = solverOpenExpression subwit $ bisubstituteUnifier bisub uval

runUnifier ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground)
    => DolanUnifier ground a
    -> WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground) a
runUnifier (ClosedExpression a) = return a
runUnifier (OpenExpression (LEBisubstitutionWitness oldvar (ptw :: _ polarity pt) False) expr) =
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(MeetType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    case polarityType @polarity of
                        NegativeType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
                        PositiveType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
                                (do
                                     tq <- invertTypeM ptw
                                     return $ joinMeetShimWit (varDolanShimWit newvar) tq)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' meet2
runUnifier (OpenExpression (GEBisubstitutionWitness oldvar (ptw :: _ polarity pt) False) expr) =
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(JoinType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    case polarityType @polarity of
                        PositiveType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
                        NegativeType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
                                (do
                                     tq <- invertTypeM ptw
                                     return $ joinMeetShimWit (varDolanShimWit newvar) tq)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' join2
runUnifier (OpenExpression (LEBisubstitutionWitness (oldvar :: SymbolType oldname) (ptw :: _ polarity pt) True) expr) = do
    newvarname <- lift $ renamerGenerate []
    newUVar newvarname $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(MeetType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    case polarityType @polarity of
                        NegativeType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $
                                 singleDolanShimWit $
                                 recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
                        PositiveType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
                                (do
                                     tq <- invertTypeM ptw
                                     return $
                                         singleDolanShimWit $
                                         recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) tq)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' meet2
runUnifier (OpenExpression (GEBisubstitutionWitness (oldvar :: SymbolType oldname) (ptw :: _ polarity pt) True) expr) = do
    newvarname <- lift $ renamerGenerate []
    newUVar newvarname $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(JoinType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    case polarityType @polarity of
                        PositiveType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $
                                 singleDolanShimWit $
                                 recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
                        NegativeType ->
                            mkPolarBisubstitution
                                False
                                oldvar
                                (return $
                                 singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
                                (do
                                     tq <- invertTypeM ptw
                                     return $
                                         singleDolanShimWit $
                                         recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) tq)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' join2

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = DolanUnifier ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ runSolver $ unifyTypes tq tp
    solveUnifier u = fmap (\(a, subs) -> (a, reverse subs)) $ runWriterT $ runUnifier u
    unifierPosSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t
