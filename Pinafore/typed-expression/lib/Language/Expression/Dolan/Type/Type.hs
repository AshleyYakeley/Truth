{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Type.Type
    ( IsDolanGroundType(..)
    , SomeGroundType(..)
    , DebugIsDolanGroundType
    , DolanShimWit
    , DolanIsoShimWit
    , DolanType(..)
    , DolanGroundedType(..)
    , DolanGroundedShimWit
    , DolanSingularType(..)
    , RecursiveTypeError(..)
    , safeRecursiveDolanSingularType
    , singularsToAnyType
    , typeToAnySingulars
    , DolanOpenExpression
    , DolanTypeCheckM
    , showDolanSingularType
    , showDolanType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Shim
import Language.Expression.Dolan.TypeSystem
import Shapes

class ( IsDolanPolyShim (DolanPolyShim ground)
      , Ord (DolanVarID ground)
      , Show (DolanVarID ground)
      , MonadThrow PatternError (DolanM ground)
      , MonadThrow (NamedExpressionError (DolanVarID ground) (DolanShimWit ground 'Negative)) (DolanM ground)
      , AllConstraint Show (DolanSingularType ground 'Positive)
      , AllConstraint Show (DolanSingularType ground 'Negative)
      , AllConstraint Show (DolanType ground 'Positive)
      , AllConstraint Show (DolanType ground 'Negative)
      --, DebugIsDolanGroundType ground
      ) => IsDolanGroundType (ground :: GroundTypeKind) where
    type DolanVarID ground :: Type
    type DolanM ground :: Type -> Type
    groundTypeVarianceType :: forall (dv :: CCRVariances) (t :: CCRVariancesKind dv). ground dv t -> CCRVariancesType dv
    groundTypeVarianceMap :: forall (dv :: CCRVariances) (t :: CCRVariancesKind dv). ground dv t -> CCRVariancesMap dv t
    groundTypeTestEquality ::
           forall (dva :: CCRVariances) (ta :: CCRVariancesKind dva) (dvb :: CCRVariances) (tb :: CCRVariancesKind dvb).
           ground dva ta
        -> ground dvb tb
        -> Maybe (dva :~: dvb, ta :~~: tb)

type SomeGroundType :: GroundTypeKind -> Type
data SomeGroundType ground =
    forall (dv :: CCRVariances) (t :: CCRVariancesKind dv). MkSomeGroundType (ground dv t)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Eq (SomeGroundType ground) where
    MkSomeGroundType ta == MkSomeGroundType tb = isJust $ groundTypeTestEquality ta tb

instance forall (ground :: GroundTypeKind). DebugIsDolanGroundType ground => Show (SomeGroundType ground) where
    show (MkSomeGroundType t) = show t

type DebugIsDolanGroundType :: GroundTypeKind -> Constraint
class ( MonadException (DolanM ground)
      , Show (Exc (DolanM ground))
      , MonadIO (DolanM ground)
      , forall dv (gt :: CCRVariancesKind dv). Show (ground dv gt)
      , forall polarity t. Is PolarityType polarity => Show (DolanType ground polarity t)
      , forall polarity t. Is PolarityType polarity => Show (DolanGroundedType ground polarity t)
      , DolanPolyShim ground Type ~ JMShim Type
      ) => DebugIsDolanGroundType ground

instance forall (ground :: GroundTypeKind). ( MonadException (DolanM ground)
         , Show (Exc (DolanM ground))
         , MonadIO (DolanM ground)
         , forall dv (gt :: CCRVariancesKind dv). Show (ground dv gt)
         , forall polarity t. Is PolarityType polarity => Show (DolanType ground polarity t)
         , forall polarity t. Is PolarityType polarity => Show (DolanGroundedType ground polarity t)
         , DolanPolyShim ground Type ~ JMShim Type
         ) => DebugIsDolanGroundType ground

type DolanShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanShimWit ground polarity = PShimWit (DolanShim ground) (DolanType ground) polarity

type DolanIsoShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanIsoShimWit ground polarity = PShimWit (DolanPolyIsoShim ground Type) (DolanType ground) polarity

type DolanType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanType ground polarity t where
    NilDolanType :: forall (ground :: GroundTypeKind) polarity. DolanType ground polarity (LimitType polarity)
    ConsDolanType
        :: forall (ground :: GroundTypeKind) polarity t1 tr.
           DolanSingularType ground polarity t1
        -> DolanType ground polarity tr
        -> DolanType ground polarity (JoinMeetType polarity t1 tr)

instance forall (ground :: GroundTypeKind) polarity t. FreeTypeVariables (DolanType ground polarity t) where
    freeTypeVariables NilDolanType = mempty
    freeTypeVariables (ConsDolanType t1 tr) = freeTypeVariables t1 <> freeTypeVariables tr

type DolanGroundedType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanGroundedType ground polarity t where
    MkDolanGroundedType
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: CCRVariances) gt t.
           ground dv gt
        -> CCRPolarArguments dv (DolanType ground) gt polarity t
        -> DolanGroundedType ground polarity t

instance forall (ground :: GroundTypeKind) polarity t. FreeTypeVariables (DolanGroundedType ground polarity t) where
    freeTypeVariables (MkDolanGroundedType _ args) = freeTypeVariables args

type DolanGroundedShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanGroundedShimWit ground polarity = PShimWit (DolanShim ground) (DolanGroundedType ground) polarity

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVarT') for type variables.
type DolanSingularType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanSingularType ground polarity t where
    GroundedDolanSingularType
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) t.
           DolanGroundedType ground polarity t
        -> DolanSingularType ground polarity t
    VarDolanSingularType
        :: forall (ground :: GroundTypeKind) polarity t. TypeVarT t -> DolanSingularType ground polarity t
    -- RecursiveDolanSingularType represents equirecursive type using type families,
    -- similar to https://semantic.org/post/forbidden-haskell-types/
    RecursiveDolanSingularType
        :: forall (ground :: GroundTypeKind) polarity t.
           TypeVarT t
        -> DolanType ground polarity t
        -> DolanSingularType ground polarity t

instance forall (ground :: GroundTypeKind) polarity t. FreeTypeVariables (DolanSingularType ground polarity t) where
    freeTypeVariables (GroundedDolanSingularType t) = freeTypeVariables t
    freeTypeVariables (VarDolanSingularType n) = freeTypeVariables n
    freeTypeVariables (RecursiveDolanSingularType v t) = difference (freeTypeVariables t) (freeTypeVariables v)

data RecursiveTypeError
    = ImmediateRecursiveTypeError (Some TypeVarT)
    | ContravariantRecursiveTypeError (Some TypeVarT)

instance Show RecursiveTypeError where
    show (ImmediateRecursiveTypeError v) = "immediate " <> show v
    show (ContravariantRecursiveTypeError v) = "contravariant " <> show v

safeRecursiveDolanSingularType ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => TypeVarT t
    -> DolanType ground polarity t
    -> Result RecursiveTypeError (DolanSingularType ground polarity t)
safeRecursiveDolanSingularType var tt = do
    let
        checkArgument ::
               forall sv pol a. Bool -> CCRVarianceType sv -> CCRPolarArgument (DolanType ground) pol sv a -> Maybe ()
        checkArgument contrv CoCCRVarianceType (CoCCRPolarArgument t) = checkType contrv t
        checkArgument contrv ContraCCRVarianceType (ContraCCRPolarArgument t) = checkType (not contrv) t
        checkArgument contrv RangeCCRVarianceType (RangeCCRPolarArgument p q) = do
            checkType (not contrv) p
            checkType contrv q
        checkArguments ::
               forall dv gt pol a.
               Bool
            -> CCRVariancesType dv
            -> CCRPolarArguments dv (DolanType ground) gt pol a
            -> Maybe ()
        checkArguments _ NilListType NilCCRArguments = return ()
        checkArguments contrv (ConsListType svt dvt) (ConsCCRArguments arg args) = do
            checkArgument contrv svt arg
            checkArguments contrv dvt args
        checkGroundedType :: forall pol a. Bool -> DolanGroundedType ground pol a -> Maybe ()
        checkGroundedType contrv (MkDolanGroundedType gt args) = checkArguments contrv (groundTypeVarianceType gt) args
        checkSingularType :: forall pol a. Bool -> DolanSingularType ground pol a -> Maybe ()
        checkSingularType contrv (VarDolanSingularType v)
            | contrv
            , MkSome v == MkSome var = Nothing
        checkSingularType _ (VarDolanSingularType _) = return ()
        checkSingularType _ (RecursiveDolanSingularType v _)
            | MkSome v == MkSome var = return ()
        checkSingularType contrv (RecursiveDolanSingularType _ t) = checkType contrv t
        checkSingularType contrv (GroundedDolanSingularType t) = checkGroundedType contrv t
        checkType :: forall pol a. Bool -> DolanType ground pol a -> Maybe ()
        checkType _ NilDolanType = return ()
        checkType contrv (ConsDolanType t1 tr) = do
            checkSingularType contrv t1
            checkType contrv tr
    case checkType False tt of
        Just () -> return ()
        Nothing -> throwExc $ ContravariantRecursiveTypeError $ MkSome var
    let
        checkImmediateSingular :: forall a. DolanSingularType ground polarity a -> Maybe ()
        checkImmediateSingular (VarDolanSingularType v)
            | MkSome v == MkSome var = Nothing
        checkImmediateSingular (RecursiveDolanSingularType _ t) = checkImmediateUses t
        checkImmediateSingular _ = return ()
        checkImmediateUses :: forall a. DolanType ground polarity a -> Maybe ()
        checkImmediateUses NilDolanType = return ()
        checkImmediateUses (ConsDolanType t1 tr) = do
            checkImmediateSingular t1
            checkImmediateUses tr
    case checkImmediateUses tt of
        Just () -> return ()
        Nothing -> throwExc $ ImmediateRecursiveTypeError $ MkSome var
    return $ RecursiveDolanSingularType var tt

singularsToAnyType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity).
       [Some (DolanSingularType ground polarity)]
    -> Some (DolanType ground polarity)
singularsToAnyType [] = MkSome NilDolanType
singularsToAnyType (MkSome s:ss) =
    case singularsToAnyType ss of
        MkSome t -> MkSome $ ConsDolanType s t

typeToAnySingulars ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) t.
       DolanType ground polarity t
    -> [Some (DolanSingularType ground polarity)]
typeToAnySingulars NilDolanType = []
typeToAnySingulars (ConsDolanType s t) = MkSome s : typeToAnySingulars t

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Semigroup (Some (DolanType ground polarity)) where
    MkSome NilDolanType <> tb = tb
    MkSome (ConsDolanType ta tr) <> tb =
        case MkSome tr <> tb of
            MkSome trb -> MkSome $ ConsDolanType ta trb

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Monoid (Some (DolanType ground polarity)) where
    mappend = (<>)
    mempty = MkSome NilDolanType

type DolanOpenExpression :: GroundTypeKind -> Type -> Type
type DolanOpenExpression ground = TSOpenExpression (DolanTypeSystem ground)

type DolanTypeCheckM :: GroundTypeKind -> Type -> Type
type DolanTypeCheckM ground = VarRenamerT (DolanTypeSystem ground) (DolanM ground)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TypeSystem (DolanTypeSystem ground) where
    type TSOuter (DolanTypeSystem ground) = DolanTypeCheckM ground
    type TSNegWitness (DolanTypeSystem ground) = DolanType ground 'Negative
    type TSPosWitness (DolanTypeSystem ground) = DolanType ground 'Positive
    type TSShim (DolanTypeSystem ground) = DolanShim ground
    type TSVarID (DolanTypeSystem ground) = DolanVarID ground

showDolanSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> String
showDolanSingularType =
    case polarityType @polarity of
        PositiveType -> allShow
        NegativeType -> allShow

showDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> String
showDolanType =
    case polarityType @polarity of
        PositiveType -> allShow
        NegativeType -> allShow

getCCRArgumentMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) polarity (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> CCRVariancesKind dv) (a :: CCRVarianceKind sv).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRVariation sv f
    -> CCRPolarArgument (DolanType ground) polarity sv a
    -> CCRPolarArguments dv (DolanType ground) (f a) polarity t
    -> VarMapping t
getCCRArgumentMapping svm (CoCCRPolarArgument t) args =
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm aa)) $ getVarMapping t
getCCRArgumentMapping svm (ContraCCRPolarArgument t) args =
    withInvertPolarity @polarity $
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm $ MkCatDual aa)) $ invertVarMapping $ getVarMapping t
getCCRArgumentMapping svm (RangeCCRPolarArgument tp tq) args =
    withInvertPolarity @polarity $
    joinVarMapping
        (\pp qq -> ccrArgumentsEndo args (ccrvMap svm $ MkCatRange pp qq))
        (invertVarMapping $ getVarMapping tp)
        (getVarMapping tq)

getCCRArgumentsMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) polarity dv gt.
       (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRVariancesMap dv gt
    -> CCRPolarArguments dv (DolanType ground) gt polarity t
    -> VarMapping t
getCCRArgumentsMapping NilCCRVariancesMap NilCCRArguments = mempty
getCCRArgumentsMapping (ConsCCRVariancesMap ccrv dvm) (ConsCCRArguments arg args) =
    getCCRArgumentMapping ccrv arg args <> getCCRArgumentsMapping dvm args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             HasVarMapping (DolanGroundedType ground polarity) where
    getVarMapping (MkDolanGroundedType gt args) = getCCRArgumentsMapping (groundTypeVarianceMap gt) args

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             HasVarMapping (DolanSingularType ground polarity) where
    getVarMapping (VarDolanSingularType var) = varVarMapping var
    getVarMapping (GroundedDolanSingularType t) = getVarMapping t
    getVarMapping (RecursiveDolanSingularType nr t) = let
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

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             HasVarMapping (DolanType ground polarity) where
    getVarMapping NilDolanType = mempty
    getVarMapping (ConsDolanType t tt) =
        case polarityType @polarity of
            PositiveType -> joinVarMapping iJoinPair (getVarMapping t) (getVarMapping tt)
            NegativeType -> joinVarMapping iMeetPair (getVarMapping t) (getVarMapping tt)
