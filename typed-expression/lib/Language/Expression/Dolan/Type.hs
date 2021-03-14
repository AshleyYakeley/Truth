{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type IsDolanPolyShim :: PolyShimKind -> Constraint
type IsDolanPolyShim pshim = (DolanVarianceInCategory pshim, LazyCategory (pshim Type), CartesianShim (pshim Type))

class ( IsDolanPolyShim (DolanPolyShim ground)
      , Ord (DolanName ground)
      , Show (DolanName ground)
      , MonadPlus (DolanM ground)
      , AllWitnessConstraint Show (DolanType ground 'Positive)
      , AllWitnessConstraint Show (DolanType ground 'Negative)
      ) => IsDolanGroundType (ground :: GroundTypeKind) where
    type DolanName ground :: Type
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
type DolanShimWit ground polarity = PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity

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

instance forall (ground :: GroundTypeKind) polarity. IsDolanGroundType ground =>
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
    GroundDolanSingularType
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) t ta.
           ground dv t
        -> DolanArguments dv (DolanType ground) t polarity ta
        -> DolanSingularType ground polarity ta
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

instance forall (ground :: GroundTypeKind) polarity. IsDolanGroundType ground =>
             TestEquality (DolanSingularType ground polarity) where
    testEquality (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- dolanTestEquality (groundTypeVarianceType gta) argsa argsb
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
       [AnyW (DolanSingularType ground polarity)]
    -> AnyW (DolanType ground polarity)
singularsToAnyType [] = MkAnyW NilDolanType
singularsToAnyType (MkAnyW s:ss) =
    case singularsToAnyType ss of
        MkAnyW t -> MkAnyW $ ConsDolanType s t

typeToAnySingulars ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) t.
       DolanType ground polarity t
    -> [AnyW (DolanSingularType ground polarity)]
typeToAnySingulars NilDolanType = []
typeToAnySingulars (ConsDolanType s t) = MkAnyW s : typeToAnySingulars t

type DolanSingularShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanSingularShimWit ground polarity = PShimWit (DolanPolyShim ground Type) (DolanSingularType ground) polarity

varDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) name.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => SymbolType name
    -> PShimWit shim (DolanType ground) polarity (UVarT name)
varDolanShimWit var = singleDolanShimWit $ mkShimWit $ VarDolanSingularType var

nilDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity).
       (IsDolanGroundType ground, InCategory shim, Is PolarityType polarity)
    => PShimWit shim (DolanType ground) polarity (LimitType polarity)
nilDolanShimWit = mkShimWit NilDolanType

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
unsafeDeleteVarShimWit n = assignUVar @Type @(LimitType polarity) n $ mkShimWit NilDolanType

singleDolanType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       DolanSingularType ground polarity t
    -> DolanType ground polarity (JoinMeetType polarity t (LimitType polarity))
singleDolanType st = ConsDolanType st NilDolanType

dolanTypeToSingular ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       DolanType ground polarity t
    -> Maybe (AnyW (DolanSingularType ground polarity))
dolanTypeToSingular (ConsDolanType st NilDolanType) = Just $ MkAnyW st
dolanTypeToSingular _ = Nothing

singleDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) (t :: Type).
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity)
    => PShimWit shim (DolanSingularType ground) polarity t
    -> PShimWit shim (DolanType ground) polarity t
singleDolanShimWit (MkShimWit st conv) = ccontramap conv $ MkShimWit (singleDolanType st) iPolarR1

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Semigroup (AnyW (DolanType ground polarity)) where
    MkAnyW NilDolanType <> tb = tb
    MkAnyW (ConsDolanType ta tr) <> tb =
        case MkAnyW tr <> tb of
            MkAnyW trb -> MkAnyW $ ConsDolanType ta trb

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Monoid (AnyW (DolanType ground polarity)) where
    mappend = (<>)
    mempty = MkAnyW NilDolanType

type DolanTypeCheckM :: GroundTypeKind -> Type -> Type
type DolanTypeCheckM ground = VarRenamerT (DolanTypeSystem ground) (DolanM ground)

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TypeSystem (DolanTypeSystem ground) where
    type TSOuter (DolanTypeSystem ground) = DolanTypeCheckM ground
    type TSNegWitness (DolanTypeSystem ground) = DolanType ground 'Negative
    type TSPosWitness (DolanTypeSystem ground) = DolanType ground 'Positive
    type TSShim (DolanTypeSystem ground) = DolanPolyShim ground Type
    type TSName (DolanTypeSystem ground) = DolanName ground

showDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> String
showDolanType =
    case polarityType @polarity of
        PositiveType -> showAllWitness
        NegativeType -> showAllWitness
