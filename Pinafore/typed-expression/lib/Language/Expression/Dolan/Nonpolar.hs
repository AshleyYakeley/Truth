module Language.Expression.Dolan.Nonpolar
    ( NonpolarDolanType
    , NonpolarShimWit
    , nonpolarToDolanType
    , dolanTypeToNonpolar
    , nonpolarTypeFreeVariables
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type NonpolarDolanType :: GroundTypeKind -> Type -> Type
data NonpolarDolanType ground t where
    GroundedNonpolarType
        :: forall (ground :: GroundTypeKind) dv (gt :: DolanVarianceKind dv) t.
           ground dv gt
        -> NonpolarArguments ground dv gt t
        -> NonpolarDolanType ground t
    VarNonpolarType :: forall (ground :: GroundTypeKind) name. SymbolType name -> NonpolarDolanType ground (UVarT name)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (NonpolarDolanType ground) where
    testEquality (GroundedNonpolarType gta argsa) (GroundedNonpolarType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- testEquality argsa argsb
        return Refl
    testEquality (VarNonpolarType na) (VarNonpolarType nb) = do
        Refl <- testEquality na nb
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

argFreeVariables :: forall (ground :: GroundTypeKind) sv t. NonpolarArgument ground sv t -> [AnyW SymbolType]
argFreeVariables (CoNonpolarArgument arg) = nonpolarTypeFreeVariables arg
argFreeVariables (ContraNonpolarArgument arg) = nonpolarTypeFreeVariables arg
argFreeVariables (RangeNonpolarArgument argp argq) = nonpolarTypeFreeVariables argp <> nonpolarTypeFreeVariables argq

nonpolarArgumentsFreeVariables ::
       forall (ground :: GroundTypeKind) dv (gt :: DolanVarianceKind dv) t.
       NonpolarArguments ground dv gt t
    -> [AnyW SymbolType]
nonpolarArgumentsFreeVariables NilCCRArguments = []
nonpolarArgumentsFreeVariables (ConsCCRArguments targ targs) =
    nonpolarArgumentsFreeVariables @ground targs <> argFreeVariables @ground targ

nonpolarTypeFreeVariables :: forall (ground :: GroundTypeKind) t. NonpolarDolanType ground t -> [AnyW SymbolType]
nonpolarTypeFreeVariables (VarNonpolarType n) = [MkAnyW n]
nonpolarTypeFreeVariables (GroundedNonpolarType _ args) = nonpolarArgumentsFreeVariables args

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

nonpolarToDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarDolanType ground t
    -> DolanShimWit ground polarity t
nonpolarToDolanType (VarNonpolarType n) = singleDolanShimWit $ mkShimWit $ VarDolanSingularType n
nonpolarToDolanType (GroundedNonpolarType gt args) =
    case mapCCRArguments nonpolarToDolanArg (groundTypeVarianceMap gt) args of
        MkShimWit dargs conv -> singleDolanShimWit $ MkShimWit (GroundedDolanSingularType gt dargs) conv

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

dolanSingularTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> Maybe (NonpolarShimWit ground polarity t)
dolanSingularTypeToNonpolar (VarDolanSingularType n) = return $ mkShimWit $ VarNonpolarType n
dolanSingularTypeToNonpolar (GroundedDolanSingularType ground args) = do
    MkShimWit npargs conv <- mapCCRArgumentsM dolanArgToNonpolar (groundTypeVarianceMap ground) args
    return $ MkShimWit (GroundedNonpolarType ground npargs) conv
dolanSingularTypeToNonpolar (RecursiveDolanSingularType _ _) = empty

dolanTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> Maybe (NonpolarShimWit ground polarity t)
dolanTypeToNonpolar t = do
    MkShimWit st conv <- dolanTypeToSingular t
    st' <- dolanSingularTypeToNonpolar st
    return $ mapPolarShimWit conv st'
