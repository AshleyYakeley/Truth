{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Nonpolar
    ( NonpolarDolanType(..)
    , NonpolarArgument(..)
    , NonpolarArguments
    , NonpolarShimWit
    , groundedNonpolarToDolanType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes
import Shapes.Unsafe

type NonpolarDolanType :: GroundTypeKind -> Type -> Type
data NonpolarDolanType ground t where
    GroundedNonpolarType
        :: forall (ground :: GroundTypeKind) dv (gt :: DolanVarianceKind dv) t.
           ground dv gt
        -> NonpolarArguments ground dv gt t
        -> NonpolarDolanType ground t
    VarNonpolarType :: forall (ground :: GroundTypeKind) name. SymbolType name -> NonpolarDolanType ground (UVarT name)
    RecursiveNonpolarType
        :: forall (ground :: GroundTypeKind) name.
           SymbolType name
        -> NonpolarDolanType ground (UVarT name)
        -> NonpolarDolanType ground (UVarT name)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (NonpolarDolanType ground) where
    testEquality (GroundedNonpolarType gta argsa) (GroundedNonpolarType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- testEquality argsa argsb
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
        :: forall (ground :: GroundTypeKind) t. NonpolarDolanType ground t -> NonpolarArgument ground CoCCRVariance t
    ContraNonpolarArgument
        :: forall (ground :: GroundTypeKind) t.
           NonpolarDolanType ground t
        -> NonpolarArgument ground ContraCCRVariance t
    RangeNonpolarArgument
        :: forall (ground :: GroundTypeKind) p q.
           NonpolarDolanType ground p
        -> NonpolarDolanType ground q
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
type NonpolarShimWit ground polarity = PolarShimWit (DolanPolyShim ground Type) (NonpolarDolanType ground) polarity

type NonpolarArgumentShimWit :: GroundTypeKind -> Polarity -> CCRArgumentKind
type NonpolarArgumentShimWit ground polarity sv
     = CCRArgumentShimWit (DolanPolyShim ground Type) (NonpolarArgument ground) polarity sv

type NonpolarArguments :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
type NonpolarArguments ground = CCRArguments (NonpolarArgument ground)

instance forall (ground :: GroundTypeKind) sv t. FreeTypeVariables (NonpolarArgument ground sv t) where
    freeTypeVariables (CoNonpolarArgument arg) = freeTypeVariables arg
    freeTypeVariables (ContraNonpolarArgument arg) = freeTypeVariables arg
    freeTypeVariables (RangeNonpolarArgument argp argq) = freeTypeVariables argp <> freeTypeVariables argq

instance forall (ground :: GroundTypeKind) t. FreeTypeVariables (NonpolarDolanType ground t) where
    freeTypeVariables (VarNonpolarType n) = freeTypeVariables n
    freeTypeVariables (GroundedNonpolarType _ args) = freeTypeVariables args
    freeTypeVariables (RecursiveNonpolarType v t) = difference (freeTypeVariables t) (freeTypeVariables v)

nonpolarToDolanArg ::
       forall (ground :: GroundTypeKind) polarity sv t. (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarArgument ground sv t
    -> CCRArgumentShimWit (DolanPolyShim ground Type) (CCRPolarArgument (DolanType ground) polarity) polarity sv t
nonpolarToDolanArg (CoNonpolarArgument t) =
    case nonpolarToDolanType t of
        MkShimWit arg conv -> MkShimWit (CoCCRPolarArgument arg) conv
nonpolarToDolanArg (ContraNonpolarArgument t) =
    invertPolarity @polarity $
    case nonpolarToDolanType t of
        MkShimWit arg conv -> MkShimWit (ContraCCRPolarArgument arg) $ MkCatDual $ uninvertPolarMap conv
nonpolarToDolanArg (RangeNonpolarArgument p q) =
    invertPolarity @polarity $
    case (nonpolarToDolanType p, nonpolarToDolanType q) of
        (MkShimWit argp convp, MkShimWit argq convq) ->
            MkShimWit (RangeCCRPolarArgument argp argq) $ MkCatRange (uninvertPolarMap convp) convq

nonpolarToDolanArguments ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanVarianceMap dv gt
    -> NonpolarArguments ground dv gt t
    -> DolanArgumentsShimWit (DolanPolyShim ground) dv (DolanType ground) gt polarity t
nonpolarToDolanArguments = mapCCRArguments nonpolarToDolanArg

groundedNonpolarToDolanType ::
       forall (ground :: GroundTypeKind) polarity dv (gt :: DolanVarianceKind dv) t.
       (IsDolanGroundType ground, Is PolarityType polarity)
    => ground dv gt
    -> NonpolarArguments ground dv gt t
    -> DolanGroundedShimWit ground polarity t
groundedNonpolarToDolanType gt args =
    case nonpolarToDolanArguments (groundTypeVarianceMap gt) args of
        MkShimWit dargs conv -> MkShimWit (MkDolanGroundedType gt dargs) conv

nonpolarToDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarDolanType ground t
    -> DolanShimWit ground polarity t
nonpolarToDolanType (VarNonpolarType n) = shimWitToDolan $ mkShimWit $ VarDolanSingularType n
nonpolarToDolanType (GroundedNonpolarType gt args) = shimWitToDolan $ groundedNonpolarToDolanType gt args
nonpolarToDolanType (RecursiveNonpolarType v t) = shimWitToDolan $ recursiveDolanShimWit v $ nonpolarToDolanType t

dolanArgToNonpolar ::
       forall (ground :: GroundTypeKind) polarity sv t. (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRPolarArgument (DolanType ground) polarity sv t
    -> Maybe (NonpolarArgumentShimWit ground polarity sv t)
dolanArgToNonpolar (CoCCRPolarArgument t) = do
    MkShimWit arg conv <- dolanTypeToNonpolar t
    return $ MkShimWit (CoNonpolarArgument arg) conv
dolanArgToNonpolar (ContraCCRPolarArgument t) =
    invertPolarity @polarity $ do
        MkShimWit arg conv <- dolanTypeToNonpolar t
        return $ MkShimWit (ContraNonpolarArgument arg) $ MkCatDual $ uninvertPolarMap conv
dolanArgToNonpolar (RangeCCRPolarArgument p q) =
    invertPolarity @polarity $ do
        MkShimWit argp convp <- dolanTypeToNonpolar p
        MkShimWit argq convq <- dolanTypeToNonpolar q
        return $ MkShimWit (RangeNonpolarArgument argp argq) $ MkCatRange (uninvertPolarMap convp) convq

dolanGroundedTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanGroundedType ground polarity t
    -> Maybe (NonpolarShimWit ground polarity t)
dolanGroundedTypeToNonpolar (MkDolanGroundedType ground args) = do
    MkShimWit npargs conv <- mapCCRArgumentsM dolanArgToNonpolar (groundTypeVarianceMap ground) args
    return $ MkShimWit (GroundedNonpolarType ground npargs) conv

getArgumentMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv).
       IsDolanGroundType ground
    => TSOpenExpression (DolanTypeSystem ground) (CCRVariation sv f)
    -> NonpolarArgument ground sv a
    -> NonpolarArguments ground dv (f a) t
    -> VarMapping (TSOpenExpression (DolanTypeSystem ground)) t
getArgumentMapping esvm (CoNonpolarArgument t) args =
    mapVarMapping (fmap (\svm aa -> ccrArgumentsEndo args $ ccrvMap svm aa) esvm) $ getVarMapping t
getArgumentMapping esvm (ContraNonpolarArgument t) args =
    mapVarMapping (fmap (\svm aa -> ccrArgumentsEndo args $ ccrvMap svm $ MkCatDual aa) esvm) $
    invertVarMapping $ getVarMapping t
getArgumentMapping esvm (RangeNonpolarArgument tp tq) args =
    joinVarMapping
        (fmap (\svm pp qq -> ccrArgumentsEndo args $ ccrvMap svm $ MkCatRange pp qq) esvm)
        (invertVarMapping $ getVarMapping tp)
        (getVarMapping tq)

getArgumentsMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) dv gt. IsDolanGroundType ground
    => TSOpenExpression (DolanTypeSystem ground) (DolanVarianceMap dv gt)
    -> NonpolarArguments ground dv gt t
    -> VarMapping (TSOpenExpression (DolanTypeSystem ground)) t
getArgumentsMapping _ NilCCRArguments = mempty
getArgumentsMapping edvm (ConsCCRArguments arg args) =
    getArgumentMapping (fmap (\(ConsDolanVarianceMap ccrv _dvm) -> ccrv) edvm) arg args <>
    getArgumentsMapping (fmap (\(ConsDolanVarianceMap _ccrv dvm) -> dvm) edvm) args

instance forall (ground :: GroundTypeKind) expr. ( IsDolanGroundType ground
         , expr ~ TSOpenExpression (DolanTypeSystem ground)
         ) => HasVarMapping expr (NonpolarDolanType ground) where
    getVarMapping (VarNonpolarType var) = varVarMapping var
    getVarMapping (GroundedNonpolarType gt args) = getArgumentsMapping (groundTypeVarianceMap gt) args
    getVarMapping (RecursiveNonpolarType nr t) = let
        vm = getVarMapping t
        in case runVarMapping vm CoVarianceType nr of
               Just emr ->
                   MkVarMapping $ \v na ->
                       getCompose $ do
                           ma <- Compose $ runVarMapping vm v na
                           mr <- Compose $ Just emr
                           return $
                               mkMapping $ \vv -> let
                                   tt = runMapping ma vv . runMapping mr tt
                                   in tt
               Nothing -> vm

mapNonpolarType ::
       forall (ground :: GroundTypeKind) n t. IsDolanGroundType ground
    => NonpolarDolanType ground t
    -> SymbolType n
    -> (UVarT n -> UVarT n)
    -> t
    -> t
mapNonpolarType wt n =
    runMapping $ fromMaybe (error "mapNonpolarType: wrong polarity") $ runVarMapping (getVarMapping wt) CoVarianceType n

type F :: Type -> Type
type family F a

withEqualVar :: SymbolType na -> SymbolType nb -> (na ~ nb => r) -> r
withEqualVar va vb call =
    case testEquality va vb of
        Just Refl -> call
        Nothing -> error "withEqualVar"

mapNonpolarType1 ::
       forall (ground :: GroundTypeKind) na nb. IsDolanGroundType ground
    => NonpolarDolanType ground (F (UVarT na))
    -> SymbolType na
    -> SymbolType nb
    -> (UVarT na -> UVarT nb)
    -> F (UVarT na)
    -> F (UVarT nb)
mapNonpolarType1 tw va vb = withEqualVar va vb $ mapNonpolarType @ground tw va

mapNonpolarType2 ::
       forall (ground :: GroundTypeKind) na nb. IsDolanGroundType ground
    => NonpolarDolanType ground (F (UVarT na))
    -> SymbolType na
    -> SymbolType nb
    -> (UVarT nb -> UVarT na)
    -> F (UVarT nb)
    -> F (UVarT na)
mapNonpolarType2 tw va vb = withEqualVar va vb $ mapNonpolarType @ground tw va

remapWit ::
       forall (ground :: GroundTypeKind) na nb.
       SymbolType na
    -> SymbolType nb
    -> NonpolarDolanType ground (F (UVarT na))
    -> NonpolarDolanType ground (F (UVarT nb))
remapWit va vb = withEqualVar va vb id

recursiveNonpolarShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name.
       (IsDolanGroundType ground, FunctionShim (pshim Type), Is PolarityType polarity)
    => SymbolType name
    -> PolarShimWit (pshim Type) (NonpolarDolanType ground) polarity (UVarT name)
    -> PolarShimWit (pshim Type) (NonpolarDolanType ground) polarity (UVarT name)
recursiveNonpolarShimWit oldvar (MkShimWit tw (MkPolarMap conv)) =
    unsafeAssignWitT @(F (UVarT name)) tw $
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        unsafeAssign @Type @(UVarT newname) @(F (UVarT newname)) $
        case polarityType @polarity of
            PositiveType -> let
                rconv :: UVarT name -> UVarT newname
                rconv = mapNonpolarType1 tw oldvar newvar rconv . shimToFunction conv
                in MkShimWit (RecursiveNonpolarType newvar $ remapWit oldvar newvar tw) $
                   MkPolarMap $ functionToShim "recursive" rconv
            NegativeType -> let
                rconv :: UVarT newname -> UVarT name
                rconv = shimToFunction conv . mapNonpolarType2 tw oldvar newvar rconv
                in MkShimWit (RecursiveNonpolarType newvar $ remapWit oldvar newvar tw) $
                   MkPolarMap $ functionToShim "recursive" rconv

dolanSingularTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> Maybe (NonpolarShimWit ground polarity t)
dolanSingularTypeToNonpolar (VarDolanSingularType n) = return $ mkShimWit $ VarNonpolarType n
dolanSingularTypeToNonpolar (GroundedDolanSingularType t) = dolanGroundedTypeToNonpolar t
dolanSingularTypeToNonpolar (RecursiveDolanSingularType v t) = do
    nw <- dolanTypeToNonpolar t
    return $ recursiveNonpolarShimWit v nw

{-
recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => SymbolType name
    -> PShimWit (pshim Type) (DolanType ground) polarity (UVarT name)
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity (UVarT name)
-}
dolanTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> Maybe (NonpolarShimWit ground polarity t)
dolanTypeToNonpolar t = do
    MkShimWit st conv <- dolanToMaybeType t
    st' <- dolanSingularTypeToNonpolar st
    return $ mapPolarShimWit conv st'

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => NonpolarTypeSystem (DolanTypeSystem ground) where
    type TSNonpolarWitness (DolanTypeSystem ground) = NonpolarDolanType ground
    nonpolarToPositive = nonpolarToDolanType
    nonpolarToNegative = nonpolarToDolanType
    positiveToNonpolar = dolanTypeToNonpolar
    negativeToNonpolar = dolanTypeToNonpolar
