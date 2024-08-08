{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Nonpolar
    ( NonpolarGroundedType(..)
    , NonpolarType(..)
    , NonpolarArgument(..)
    , NonpolarArguments
    , NonpolarShimWit
    , groundedNonpolarToDolanType
    ) where

import Data.Shim
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Shim
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem
import Shapes
import Shapes.Unsafe

type NonpolarGroundedType :: GroundTypeKind -> Type -> Type
data NonpolarGroundedType ground t where
    MkNonpolarGroundedType
        :: forall (ground :: GroundTypeKind) dv (gt :: CCRVariancesKind dv) (t :: Type).
           ground dv gt
        -> NonpolarArguments ground dv gt t
        -> NonpolarGroundedType ground t

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (NonpolarGroundedType ground) where
    testEquality (MkNonpolarGroundedType gta argsa) (MkNonpolarGroundedType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- testEquality argsa argsb
        return Refl

type NonpolarType :: GroundTypeKind -> Type -> Type
data NonpolarType ground t where
    GroundedNonpolarType
        :: forall (ground :: GroundTypeKind) (t :: Type). NonpolarGroundedType ground t -> NonpolarType ground t
    VarNonpolarType :: forall (ground :: GroundTypeKind) tv. TypeVarT tv -> NonpolarType ground tv
    RecursiveNonpolarType
        :: forall (ground :: GroundTypeKind) tv. TypeVarT tv -> NonpolarType ground tv -> NonpolarType ground tv

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (NonpolarType ground) where
    testEquality (GroundedNonpolarType ta) (GroundedNonpolarType tb) = do
        Refl <- testEquality ta tb
        return Refl
    testEquality (VarNonpolarType na) (VarNonpolarType nb) = do
        Refl <- testEquality na nb
        return Refl
    testEquality (RecursiveNonpolarType na pta) (RecursiveNonpolarType nb ptb) = do
        Refl <- testEquality na nb
        Refl <- testEquality pta ptb
        return Refl
    testEquality _ _ = Nothing

type NonpolarArgument :: GroundTypeKind -> CCRArgumentKind
data NonpolarArgument ground sv t where
    CoNonpolarArgument
        :: forall (ground :: GroundTypeKind) t. NonpolarType ground t -> NonpolarArgument ground CoCCRVariance t
    ContraNonpolarArgument
        :: forall (ground :: GroundTypeKind) t. NonpolarType ground t -> NonpolarArgument ground ContraCCRVariance t
    RangeNonpolarArgument
        :: forall (ground :: GroundTypeKind) p q.
           NonpolarType ground p
        -> NonpolarType ground q
        -> NonpolarArgument ground 'RangeCCRVariance '( p, q)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => IsCCRArg (NonpolarArgument ground) where
    ccrArgumentType (CoNonpolarArgument _) = CoCCRVarianceType
    ccrArgumentType (ContraNonpolarArgument _) = ContraCCRVarianceType
    ccrArgumentType (RangeNonpolarArgument _ _) = RangeCCRVarianceType
    ccrArgumentTestEquality (CoNonpolarArgument a) (CoNonpolarArgument b) = do
        Refl <- testEquality a b
        return Refl
    ccrArgumentTestEquality (ContraNonpolarArgument a) (ContraNonpolarArgument b) = do
        Refl <- testEquality a b
        return Refl
    ccrArgumentTestEquality (RangeNonpolarArgument ap aq) (RangeNonpolarArgument bp bq) = do
        Refl <- testEquality ap bp
        Refl <- testEquality aq bq
        return Refl

type NonpolarShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type NonpolarShimWit ground polarity = PolarShimWit (DolanShim ground) (NonpolarType ground) polarity

type NonpolarGroundedShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type NonpolarGroundedShimWit ground polarity = PolarShimWit (DolanShim ground) (NonpolarGroundedType ground) polarity

type NonpolarArgumentShimWit :: GroundTypeKind -> Polarity -> CCRArgumentKind
type NonpolarArgumentShimWit ground polarity sv
     = CCRArgumentShimWit (DolanShim ground) (NonpolarArgument ground) polarity sv

type NonpolarArguments :: GroundTypeKind -> forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type -> Type
type NonpolarArguments ground = CCRArguments (NonpolarArgument ground)

instance forall (ground :: GroundTypeKind) sv t. FreeTypeVariables (NonpolarArgument ground sv t) where
    freeTypeVariables (CoNonpolarArgument arg) = freeTypeVariables arg
    freeTypeVariables (ContraNonpolarArgument arg) = freeTypeVariables arg
    freeTypeVariables (RangeNonpolarArgument argp argq) = freeTypeVariables argp <> freeTypeVariables argq

instance forall (ground :: GroundTypeKind) t. FreeTypeVariables (NonpolarGroundedType ground t) where
    freeTypeVariables (MkNonpolarGroundedType _ args) = freeTypeVariables args

instance forall (ground :: GroundTypeKind) t. FreeTypeVariables (NonpolarType ground t) where
    freeTypeVariables (VarNonpolarType n) = freeTypeVariables n
    freeTypeVariables (GroundedNonpolarType t) = freeTypeVariables t
    freeTypeVariables (RecursiveNonpolarType v t) = difference (freeTypeVariables t) (freeTypeVariables v)

nonpolarToDolanArg ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity sv t.
       (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => NonpolarArgument ground sv t
    -> CCRArgumentShimWit (pshim Type) (CCRPolarArgument (DolanType ground) polarity) polarity sv t
nonpolarToDolanArg (CoNonpolarArgument t) =
    case nonpolarToDolanType t of
        MkShimWit arg conv -> MkShimWit (CoCCRPolarArgument arg) conv
nonpolarToDolanArg (ContraNonpolarArgument t) =
    withInvertPolarity @polarity $
    case nonpolarToDolanType t of
        MkShimWit arg conv -> MkShimWit (ContraCCRPolarArgument arg) $ MkCatDual $ uninvertPolarShim conv
nonpolarToDolanArg (RangeNonpolarArgument p q) =
    withInvertPolarity @polarity $
    case (nonpolarToDolanType p, nonpolarToDolanType q) of
        (MkShimWit argp convp, MkShimWit argq convq) ->
            MkShimWit (RangeCCRPolarArgument argp argq) $ MkCatRange (uninvertPolarShim convp) convq

nonpolarToDolanArguments ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity dv gt t.
       (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => CCRVariancesMap dv gt
    -> NonpolarArguments ground dv gt t
    -> CCRPolarArgumentsShimWit pshim dv (DolanType ground) gt polarity t
nonpolarToDolanArguments = mapPolarCCRArguments @pshim (nonpolarToDolanArg @ground @pshim @polarity)

groundedNonpolarToDolanType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => NonpolarGroundedType ground t
    -> PShimWit (pshim Type) (DolanGroundedType ground) polarity t
groundedNonpolarToDolanType (MkNonpolarGroundedType gt args) =
    mkDolanGroundedShimWit gt $ nonpolarToDolanArguments (groundTypeVarianceMap gt) args

nonpolarToDolanType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => NonpolarType ground t
    -> PShimWit (pshim Type) (DolanType ground) polarity t
nonpolarToDolanType (VarNonpolarType n) = shimWitToDolan $ mkShimWit $ VarDolanSingularType n
nonpolarToDolanType (GroundedNonpolarType t) = shimWitToDolan $ groundedNonpolarToDolanType t
nonpolarToDolanType (RecursiveNonpolarType v t) = shimWitToDolan $ recursiveDolanShimWit v $ nonpolarToDolanType t

dolanArgToNonpolar ::
       forall (ground :: GroundTypeKind) polarity sv t. (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRPolarArgument (DolanType ground) polarity sv t
    -> Maybe (NonpolarArgumentShimWit ground polarity sv t)
dolanArgToNonpolar (CoCCRPolarArgument t) = do
    MkShimWit arg conv <- dolanTypeToNonpolar t
    return $ MkShimWit (CoNonpolarArgument arg) conv
dolanArgToNonpolar (ContraCCRPolarArgument t) =
    withInvertPolarity @polarity $ do
        MkShimWit arg conv <- dolanTypeToNonpolar t
        return $ MkShimWit (ContraNonpolarArgument arg) $ MkCatDual $ uninvertPolarShim conv
dolanArgToNonpolar (RangeCCRPolarArgument p q) =
    withInvertPolarity @polarity $ do
        MkShimWit argp convp <- dolanTypeToNonpolar p
        MkShimWit argq convq <- dolanTypeToNonpolar q
        return $ MkShimWit (RangeNonpolarArgument argp argq) $ MkCatRange (uninvertPolarShim convp) convq

dolanGroundedTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanGroundedType ground polarity t
    -> Maybe (NonpolarGroundedShimWit ground polarity t)
dolanGroundedTypeToNonpolar (MkDolanGroundedType ground args) = do
    MkShimWit npargs conv <- mapPolarCCRArgumentsM dolanArgToNonpolar (groundTypeVarianceMap ground) args
    return $ MkShimWit (MkNonpolarGroundedType ground npargs) conv

getArgumentMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> CCRVariancesKind dv) (a :: CCRVarianceKind sv).
       IsDolanGroundType ground
    => CCRVariation sv f
    -> NonpolarArgument ground sv a
    -> NonpolarArguments ground dv (f a) t
    -> VarMapping t
getArgumentMapping svm (CoNonpolarArgument t) args =
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm aa)) $ getVarMapping t
getArgumentMapping svm (ContraNonpolarArgument t) args =
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm $ MkCatDual aa)) $ invertVarMapping $ getVarMapping t
getArgumentMapping svm (RangeNonpolarArgument tp tq) args =
    joinVarMapping
        (\pp qq -> ccrArgumentsEndo args (ccrvMap svm $ MkCatRange pp qq))
        (invertVarMapping $ getVarMapping tp)
        (getVarMapping tq)

getArgumentsMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) dv gt. IsDolanGroundType ground
    => CCRVariancesMap dv gt
    -> NonpolarArguments ground dv gt t
    -> VarMapping t
getArgumentsMapping NilCCRVariancesMap NilCCRArguments = mempty
getArgumentsMapping (ConsCCRVariancesMap ccrv dvm) (ConsCCRArguments arg args) =
    getArgumentMapping ccrv arg args <> getArgumentsMapping dvm args

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => HasVarMapping (NonpolarGroundedType ground) where
    getVarMapping (MkNonpolarGroundedType gt args) = getArgumentsMapping (groundTypeVarianceMap gt) args

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => HasVarMapping (NonpolarType ground) where
    getVarMapping (VarNonpolarType var) = varVarMapping var
    getVarMapping (GroundedNonpolarType t) = getVarMapping t
    getVarMapping (RecursiveNonpolarType nr t) = let
        vm = getVarMapping t
        in case runVarMapping vm CoVarianceType nr of
               Just mr ->
                   MkVarMapping $ \v na -> do
                       ma <- runVarMapping vm v na
                       return $
                           mkMapping $ \vv -> let
                               tt = runMapping ma vv . runMapping mr tt
                               in tt
               Nothing -> vm

mapNonpolarType ::
       forall (ground :: GroundTypeKind) tv t. IsDolanGroundType ground
    => NonpolarType ground t
    -> TypeVarT tv
    -> (tv -> tv)
    -> t
    -> t
mapNonpolarType wt n =
    runMapping $ fromMaybe (error "mapNonpolarType: wrong polarity") $ runVarMapping (getVarMapping wt) CoVarianceType n

type F :: Type -> Type
type family F a

withEqualVar :: TypeVarT tva -> TypeVarT tvb -> (tva ~ tvb => r) -> r
withEqualVar va vb call =
    case testEquality va vb of
        Just Refl -> call
        Nothing -> error "withEqualVar"

mapNonpolarType1 ::
       forall (ground :: GroundTypeKind) tva tvb. IsDolanGroundType ground
    => NonpolarType ground (F tva)
    -> TypeVarT tva
    -> TypeVarT tvb
    -> (tva -> tvb)
    -> F tva
    -> F tvb
mapNonpolarType1 tw va vb = withEqualVar va vb $ mapNonpolarType @ground tw va

mapNonpolarType2 ::
       forall (ground :: GroundTypeKind) tva tvb. IsDolanGroundType ground
    => NonpolarType ground (F tva)
    -> TypeVarT tva
    -> TypeVarT tvb
    -> (tvb -> tva)
    -> F tvb
    -> F tva
mapNonpolarType2 tw va vb = withEqualVar va vb $ mapNonpolarType @ground tw va

remapWit ::
       forall (ground :: GroundTypeKind) tva tvb.
       TypeVarT tva
    -> TypeVarT tvb
    -> NonpolarType ground (F tva)
    -> NonpolarType ground (F tvb)
remapWit va vb = withEqualVar va vb id

recursiveNonpolarShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv.
       (IsDolanGroundType ground, RecoverShim (pshim Type), Is PolarityType polarity)
    => TypeVarT tv
    -> PolarShimWit (pshim Type) (NonpolarType ground) polarity tv
    -> PolarShimWit (pshim Type) (NonpolarType ground) polarity tv
recursiveNonpolarShimWit oldvar (MkShimWit tw (MkPolarShim conv)) =
    unsafeAssignWitT @(F tv) tw $
    newTypeVar (typeVarName oldvar) $ \(newvar :: TypeVarT newtv) ->
        unsafeAssign @Type @newtv @(F newtv) $
        case polarityType @polarity of
            PositiveType -> let
                rconv :: tv -> newtv
                rconv = mapNonpolarType1 tw oldvar newvar rconv . shimToFunction conv
                in MkShimWit (RecursiveNonpolarType newvar $ remapWit oldvar newvar tw) $
                   MkPolarShim $ functionToShim "recursive" rconv
            NegativeType -> let
                rconv :: newtv -> tv
                rconv = shimToFunction conv . mapNonpolarType2 tw oldvar newvar rconv
                in MkShimWit (RecursiveNonpolarType newvar $ remapWit oldvar newvar tw) $
                   MkPolarShim $ functionToShim "recursive" rconv

dolanSingularTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> Maybe (NonpolarShimWit ground polarity t)
dolanSingularTypeToNonpolar (VarDolanSingularType n) = return $ mkShimWit $ VarNonpolarType n
dolanSingularTypeToNonpolar (GroundedDolanSingularType t) =
    fmap (chainShimWit $ \gnt -> mkShimWit $ GroundedNonpolarType gnt) $ dolanGroundedTypeToNonpolar t
dolanSingularTypeToNonpolar (RecursiveDolanSingularType v t) = do
    nw <- dolanTypeToNonpolar t
    return $ recursiveNonpolarShimWit v nw

dolanTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> Maybe (NonpolarShimWit ground polarity t)
dolanTypeToNonpolar t = do
    MkShimWit st conv <- dolanToMaybeType t
    st' <- dolanSingularTypeToNonpolar st
    return $ mapPolarShimWit conv st'

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => NonpolarTypeSystem (DolanTypeSystem ground) where
    type TSNonpolarWitness (DolanTypeSystem ground) = NonpolarType ground
    nonpolarToPositive = nonpolarToDolanType
    nonpolarToNegative = nonpolarToDolanType
    positiveToNonpolar = dolanTypeToNonpolar
    negativeToNonpolar = dolanTypeToNonpolar
