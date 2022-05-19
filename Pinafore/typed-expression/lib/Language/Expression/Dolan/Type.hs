{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
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

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanType ground polarity) where
    testEquality NilDolanType NilDolanType = return Refl
    testEquality (ConsDolanType t1a tra) (ConsDolanType t1b trb) = do
        Refl <- testEquality t1a t1b
        Refl <- testEquality tra trb
        return Refl
    testEquality _ _ = Nothing

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVarT') for type variables.
type DolanSingularType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanSingularType ground polarity t where
    GroundedDolanSingularType
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) gt t.
           ground dv gt
        -> DolanArguments dv (DolanType ground) gt polarity t
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

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanSingularType ground polarity) where
    testEquality (GroundedDolanSingularType gta argsa) (GroundedDolanSingularType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- testEquality argsa argsb
        return Refl
    testEquality (VarDolanSingularType na) (VarDolanSingularType nb) = do
        Refl <- testEquality na nb
        return Refl
    testEquality (RecursiveDolanSingularType na pta) (RecursiveDolanSingularType nb ptb) = do
        Refl <- testEquality na nb
        Refl <- testEquality pta ptb
        return Refl
    testEquality _ _ = Nothing

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
varDolanShimWit var = singleDolanShimWit $ mkPolarShimWit $ VarDolanSingularType var

groundedDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) dv gt t.
       (IsDolanGroundType ground, JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => ground dv gt
    -> DolanArgumentsShimWit pshim dv (DolanType ground) gt polarity t
    -> PShimWit (pshim Type) (DolanType ground) polarity t
groundedDolanShimWit gt (MkShimWit args conv) = singleDolanShimWit $ MkShimWit (GroundedDolanSingularType gt args) conv

toGroundedDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) t r.
       (IsDolanGroundType ground, JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => PShimWit (pshim Type) (DolanType ground) polarity t
    -> (forall dv gt. ground dv gt -> DolanArgumentsShimWit pshim dv (DolanType ground) gt polarity t -> r)
    -> Maybe r
toGroundedDolanShimWit (MkShimWit t conv) call =
    case t of
        ConsDolanType (GroundedDolanSingularType gt args) NilDolanType ->
            Just $ call gt (MkShimWit args $ iPolarL1 . conv)
        _ -> Nothing

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

dolanTypeToSingular ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) (t :: Type).
       (JoinMeetIsoCategory shim, Is PolarityType polarity)
    => DolanType ground polarity t
    -> Maybe (PShimWit shim (DolanSingularType ground) polarity t)
dolanTypeToSingular (ConsDolanType st NilDolanType) = Just $ MkShimWit st iPolarL1
dolanTypeToSingular _ = Nothing

singleDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) (t :: Type).
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => PShimWit shim (DolanSingularType ground) polarity t
    -> PShimWit shim (DolanType ground) polarity t
singleDolanShimWit (MkShimWit st conv) = ccontramap conv $ MkShimWit (singleDolanType st) iPolarR1

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
