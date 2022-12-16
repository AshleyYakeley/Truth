{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Type
    ( BisubstitutablePolyShim(..)
    , IsDolanPolyShim
    , IsDolanGroundType(..)
    , SomeGroundType(..)
    , DebugIsDolanGroundType
    , DolanShimWit
    , DolanIsoShimWit
    , DolanType(..)
    , DolanTypeSub(..)
    , dolanToMaybeTypeShim
    , typeToSomeDolan
    , shimWitToDolan
    , dolanToMaybeShimWit
    , DolanGroundedType(..)
    , DolanGroundedShimWit
    , DolanSingularType(..)
    , safeRecursiveDolanSingularType
    , singularsToAnyType
    , typeToAnySingulars
    , DolanSingularShimWit
    , varDolanShimWit
    , nilDolanShimWit
    , consDolanShimWit
    , unsafeDeleteVarShimWit
    , singleDolanType
    , DolanTypeCheckM
    , showDolanType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type BisubstitutablePolyShim :: PolyShimKind -> Constraint
class (JoinMeetIsoCategory (pshim Type), IsoMapShim (pshim Type), DolanVarianceCategory pshim, ReduciblePolyShim pshim) =>
          BisubstitutablePolyShim pshim where
    reducedBisubstitutablePolyShim ::
           Dict (BisubstitutablePolyShim (ReducedPolyShim pshim), LazyCategory (ReducedPolyShim pshim Type))

instance forall m (pshim :: PolyShimKind). (Applicative m, BisubstitutablePolyShim pshim) =>
             BisubstitutablePolyShim (PolyComposeShim m pshim) where
    reducedBisubstitutablePolyShim =
        case reducedBisubstitutablePolyShim @pshim of
            Dict -> Dict

instance forall (pshim :: PolyShimKind). (BisubstitutablePolyShim pshim, LazyCategory (pshim Type)) =>
             BisubstitutablePolyShim (PolyIso pshim) where
    reducedBisubstitutablePolyShim =
        case reducedBisubstitutablePolyShim @pshim of
            Dict -> Dict

instance ReduciblePolyShim JMShim where
    type ReducedPolyShim JMShim = JMShim

instance BisubstitutablePolyShim JMShim where
    reducedBisubstitutablePolyShim = Dict

type IsDolanPolyShim :: PolyShimKind -> Constraint
type IsDolanPolyShim pshim
     = ( BisubstitutablePolyShim pshim
       , JoinMeetCategory (pshim Type)
       , LazyCategory (pshim Type)
       , CartesianShim (pshim Type))

class ( IsDolanPolyShim (DolanPolyShim ground)
      , Ord (DolanVarID ground)
      , Show (DolanVarID ground)
      , MonadPlus (DolanM ground)
      , MonadThrow ExpressionError (DolanM ground)
      , AllConstraint Show (DolanType ground 'Positive)
      , AllConstraint Show (DolanType ground 'Negative)
      ) => IsDolanGroundType (ground :: GroundTypeKind) where
    type DolanVarID ground :: Type
    type DolanM ground :: Type -> Type
    groundTypeVarianceType ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). ground dv t -> DolanVarianceType dv
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). ground dv t -> DolanVarianceMap dv t
    groundTypeTestEquality ::
           forall (dva :: DolanVariance) (ta :: DolanVarianceKind dva) (dvb :: DolanVariance) (tb :: DolanVarianceKind dvb).
           ground dva ta
        -> ground dvb tb
        -> Maybe (dva :~: dvb, ta :~~: tb)

type SomeGroundType :: GroundTypeKind -> Type
data SomeGroundType ground =
    forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). MkSomeGroundType (ground dv t)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Eq (SomeGroundType ground) where
    MkSomeGroundType ta == MkSomeGroundType tb = isJust $ groundTypeTestEquality ta tb

instance forall (ground :: GroundTypeKind). DebugIsDolanGroundType ground => Show (SomeGroundType ground) where
    show (MkSomeGroundType t) = show t

type DebugIsDolanGroundType :: GroundTypeKind -> Constraint
class ( IsDolanGroundType ground
      , MonadIO (DolanM ground)
      , forall dv (gt :: DolanVarianceKind dv). Show (ground dv gt)
      , forall polarity t. Is PolarityType polarity => Show (DolanType ground polarity t)
      , forall polarity t. Is PolarityType polarity => Show (DolanGroundedType ground polarity t)
      ) => DebugIsDolanGroundType ground

instance forall (ground :: GroundTypeKind). ( IsDolanGroundType ground
         , MonadIO (DolanM ground)
         , forall dv (gt :: DolanVarianceKind dv). Show (ground dv gt)
         , forall polarity t. Is PolarityType polarity => Show (DolanType ground polarity t)
         , forall polarity t. Is PolarityType polarity => Show (DolanGroundedType ground polarity t)
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

type DolanTypeSub :: GroundTypeKind -> Polarity -> (Type -> Type) -> Constraint
class Is PolarityType polarity => DolanTypeSub ground polarity w | w -> ground polarity where
    typeToDolan ::
           forall (shim :: ShimKind Type) t. JoinMeetIsoCategory shim
        => w t
        -> PShimWit shim (DolanType ground) polarity t
    dolanToMaybeType ::
           forall (shim :: ShimKind Type) t. JoinMeetIsoCategory shim
        => DolanType ground polarity t
        -> Maybe (PolarShimWit shim w polarity t)

dolanToMaybeTypeShim ::
       forall (ground :: GroundTypeKind) polarity w t.
       (DolanTypeSub ground polarity w, JoinMeetIsoCategory (DolanShim ground))
    => DolanType ground polarity t
    -> Maybe (PolarShimWit (DolanShim ground) w polarity t)
dolanToMaybeTypeShim = dolanToMaybeType

typeToSomeDolan ::
       forall (ground :: GroundTypeKind) polarity w t.
       (DolanTypeSub ground polarity w, JoinMeetIsoCategory (DolanShim ground))
    => w t
    -> Some (DolanType ground polarity)
typeToSomeDolan t = shimWitToSome $ typeToDolan @ground @polarity @w @(DolanShim ground) t

shimWitToDolan ::
       forall (ground :: GroundTypeKind) polarity w (shim :: ShimKind Type) t.
       (DolanTypeSub ground polarity w, JoinMeetIsoCategory shim)
    => PolarShimWit shim w polarity t
    -> PShimWit shim (DolanType ground) polarity t
shimWitToDolan (MkShimWit wt conv) = mapShimWit conv $ typeToDolan wt

dolanToMaybeShimWit ::
       forall (ground :: GroundTypeKind) polarity w (shim :: ShimKind Type) t.
       (DolanTypeSub ground polarity w, JoinMeetIsoCategory shim)
    => PShimWit shim (DolanType ground) polarity t
    -> Maybe (PolarShimWit shim w polarity t)
dolanToMaybeShimWit (MkShimWit wt conv) = fmap (mapShimWit conv) $ dolanToMaybeType wt

instance forall (ground :: GroundTypeKind) polarity. Is PolarityType polarity =>
             DolanTypeSub ground polarity (DolanType ground polarity) where
    typeToDolan = mkShimWit
    dolanToMaybeType = Just . mkShimWit

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanType ground polarity) where
    testEquality NilDolanType NilDolanType = return Refl
    testEquality (ConsDolanType t1a tra) (ConsDolanType t1b trb) = do
        Refl <- testEquality t1a t1b
        Refl <- testEquality tra trb
        return Refl
    testEquality _ _ = Nothing

type DolanGroundedType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanGroundedType ground polarity t where
    MkDolanGroundedType
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) gt t.
           ground dv gt
        -> DolanArguments dv (DolanType ground) gt polarity t
        -> DolanGroundedType ground polarity t

instance forall (ground :: GroundTypeKind) polarity t. FreeTypeVariables (DolanGroundedType ground polarity t) where
    freeTypeVariables (MkDolanGroundedType _ args) = freeTypeVariables args

type DolanGroundedShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanGroundedShimWit ground polarity = PShimWit (DolanShim ground) (DolanGroundedType ground) polarity

instance forall (ground :: GroundTypeKind) polarity. Is PolarityType polarity =>
             DolanTypeSub ground polarity (DolanGroundedType ground polarity) where
    typeToDolan t = typeToDolan $ GroundedDolanSingularType t
    dolanToMaybeType t = do
        MkShimWit st conv <- dolanToMaybeType t
        case st of
            GroundedDolanSingularType gt -> return $ MkShimWit gt conv
            _ -> Nothing

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVarT') for type variables.
type DolanSingularType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanSingularType ground polarity t where
    GroundedDolanSingularType
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) t.
           DolanGroundedType ground polarity t
        -> DolanSingularType ground polarity t
    VarDolanSingularType
        :: forall (ground :: GroundTypeKind) polarity name.
           SymbolType name
        -> DolanSingularType ground polarity (UVarT name)
    -- RecursiveDolanSingularType represents equirecursive type using type families,
    -- similar to https://semantic.org/post/forbidden-haskell-types/
    RecursiveDolanSingularType
        :: forall (ground :: GroundTypeKind) polarity name.
           SymbolType name
        -> DolanType ground polarity (UVarT name)
        -> DolanSingularType ground polarity (UVarT name)

instance forall (ground :: GroundTypeKind) polarity t. FreeTypeVariables (DolanSingularType ground polarity t) where
    freeTypeVariables (GroundedDolanSingularType t) = freeTypeVariables t
    freeTypeVariables (VarDolanSingularType n) = freeTypeVariables n
    freeTypeVariables (RecursiveDolanSingularType v t) = difference (freeTypeVariables t) (freeTypeVariables v)

instance forall (ground :: GroundTypeKind) polarity. Is PolarityType polarity =>
             DolanTypeSub ground polarity (DolanSingularType ground polarity) where
    typeToDolan t = MkShimWit (singleDolanType t) iPolarR1
    dolanToMaybeType (ConsDolanType t NilDolanType) = Just $ MkShimWit t iPolarL1
    dolanToMaybeType _ = Nothing

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanGroundedType ground polarity) where
    testEquality (MkDolanGroundedType gta argsa) (MkDolanGroundedType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- testEquality argsa argsb
        return Refl

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanSingularType ground polarity) where
    testEquality (GroundedDolanSingularType ta) (GroundedDolanSingularType tb) = do
        Refl <- testEquality ta tb
        return Refl
    testEquality (VarDolanSingularType na) (VarDolanSingularType nb) = do
        Refl <- testEquality na nb
        return Refl
    testEquality (RecursiveDolanSingularType na pta) (RecursiveDolanSingularType nb ptb) = do
        Refl <- testEquality na nb
        Refl <- testEquality pta ptb
        return Refl
    testEquality _ _ = Nothing

-- | OK if there are no contravariant uses
safeRecursiveDolanSingularType ::
       forall (ground :: GroundTypeKind) polarity name. IsDolanGroundType ground
    => SymbolType name
    -> DolanType ground polarity (UVarT name)
    -> Maybe (DolanSingularType ground polarity (UVarT name))
safeRecursiveDolanSingularType var tt = do
    let
        checkArgument ::
               forall sv pol t. Bool -> CCRVarianceType sv -> CCRPolarArgument (DolanType ground) pol sv t -> Maybe ()
        checkArgument contrv CoCCRVarianceType (CoCCRPolarArgument t) = checkType contrv t
        checkArgument contrv ContraCCRVarianceType (ContraCCRPolarArgument t) = checkType (not contrv) t
        checkArgument contrv RangeCCRVarianceType (RangeCCRPolarArgument p q) = do
            checkType (not contrv) p
            checkType contrv q
        checkArguments ::
               forall dv gt pol t.
               Bool
            -> DolanVarianceType dv
            -> DolanArguments dv (DolanType ground) gt pol t
            -> Maybe ()
        checkArguments _ NilListType NilCCRArguments = return ()
        checkArguments contrv (ConsListType svt dvt) (ConsCCRArguments arg args) = do
            checkArgument contrv svt arg
            checkArguments contrv dvt args
        checkGroundedType :: forall pol t. Bool -> DolanGroundedType ground pol t -> Maybe ()
        checkGroundedType contrv (MkDolanGroundedType gt args) = checkArguments contrv (groundTypeVarianceType gt) args
        checkSingularType :: forall pol t. Bool -> DolanSingularType ground pol t -> Maybe ()
        checkSingularType contrv (VarDolanSingularType v)
            | contrv
            , MkSome v == MkSome var = Nothing
        checkSingularType _ (VarDolanSingularType _) = return ()
        checkSingularType _ (RecursiveDolanSingularType v _)
            | MkSome v == MkSome var = return ()
        checkSingularType contrv (RecursiveDolanSingularType _ t) = checkType contrv t
        checkSingularType contrv (GroundedDolanSingularType t) = checkGroundedType contrv t
        checkType :: forall pol t. Bool -> DolanType ground pol t -> Maybe ()
        checkType _ NilDolanType = return ()
        checkType contrv (ConsDolanType t1 tr) = do
            checkSingularType contrv t1
            checkType contrv tr
    checkType False tt
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

type DolanSingularShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanSingularShimWit ground polarity = PShimWit (DolanShim ground) (DolanSingularType ground) polarity

varDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) name.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => SymbolType name
    -> PShimWit shim (DolanType ground) polarity (UVarT name)
varDolanShimWit var = typeToDolan $ VarDolanSingularType var

nilDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity).
       (IsDolanGroundType ground, Category shim, Is PolarityType polarity)
    => PShimWit shim (DolanType ground) polarity (LimitType polarity)
nilDolanShimWit = mkPolarShimWit NilDolanType

consDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) t1 tr.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => PShimWit shim (DolanSingularType ground) polarity t1
    -> PShimWit shim (DolanType ground) polarity tr
    -> PShimWit shim (DolanType ground) polarity (JoinMeetType polarity t1 tr)
consDolanShimWit (MkShimWit t1 conv1) (MkShimWit tr convr) = MkShimWit (ConsDolanType t1 tr) (iPolarPair conv1 convr)

unsafeDeleteVarShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) name.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => SymbolType name
    -> PShimWit shim (DolanType ground) polarity (UVarT name)
unsafeDeleteVarShimWit n = assignUVarT @(LimitType polarity) n nilDolanShimWit

singleDolanType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       DolanSingularType ground polarity t
    -> DolanType ground polarity (JoinMeetType polarity t (LimitType polarity))
singleDolanType st = ConsDolanType st NilDolanType

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

type DolanTypeCheckM :: GroundTypeKind -> Type -> Type
type DolanTypeCheckM ground = VarRenamerT (DolanTypeSystem ground) (DolanM ground)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TypeSystem (DolanTypeSystem ground) where
    type TSOuter (DolanTypeSystem ground) = DolanTypeCheckM ground
    type TSNegWitness (DolanTypeSystem ground) = DolanType ground 'Negative
    type TSPosWitness (DolanTypeSystem ground) = DolanType ground 'Positive
    type TSShim (DolanTypeSystem ground) = DolanShim ground
    type TSVarID (DolanTypeSystem ground) = DolanVarID ground

showDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> String
showDolanType =
    case polarityType @polarity of
        PositiveType -> allShow
        NegativeType -> allShow

getCCRArgumentMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) polarity (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => CCRVariation sv f
    -> CCRPolarArgument (DolanType ground) polarity sv a
    -> DolanArguments dv (DolanType ground) (f a) polarity t
    -> VarMapping t
getCCRArgumentMapping svm (CoCCRPolarArgument t) args =
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm aa)) $ getVarMapping t
getCCRArgumentMapping svm (ContraCCRPolarArgument t) args =
    invertPolarity @polarity $
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm $ MkCatDual aa)) $ invertVarMapping $ getVarMapping t
getCCRArgumentMapping svm (RangeCCRPolarArgument tp tq) args =
    invertPolarity @polarity $
    joinVarMapping
        (\pp qq -> ccrArgumentsEndo args (ccrvMap svm $ MkCatRange pp qq))
        (invertVarMapping $ getVarMapping tp)
        (getVarMapping tq)

getCCRArgumentsMapping ::
       forall (ground :: GroundTypeKind) (t :: Type) polarity dv gt.
       (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanVarianceMap dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> VarMapping t
getCCRArgumentsMapping NilDolanVarianceMap NilCCRArguments = mempty
getCCRArgumentsMapping (ConsDolanVarianceMap ccrv dvm) (ConsCCRArguments arg args) =
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
