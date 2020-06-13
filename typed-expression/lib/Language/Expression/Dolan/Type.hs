module Language.Expression.Dolan.Type where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Language.Expression.TypeVariable
import Shapes

type IsDolanPolyShim :: PolyMapKind -> Constraint
type IsDolanPolyShim pshim = (DolanVarianceInCategory pshim, Category (pshim Type), Shim (pshim Type))

type DolanType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanType ground polarity t where
    PlainDolanType
        :: forall (ground :: GroundTypeKind) polarity t. DolanPlainType ground polarity t -> DolanType ground polarity t
    RecursiveDolanType
        :: forall (ground :: GroundTypeKind) polarity name t.
           SymbolType name
        -> DolanPlainType ground polarity t
        -> DolanType ground polarity t

type DolanShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanShimWit ground polarity = PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity

type DolanPlainType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanPlainType ground polarity t where
    NilDolanPlainType :: forall (ground :: GroundTypeKind) polarity. DolanPlainType ground polarity (LimitType polarity)
    ConsDolanPlainType
        :: forall (ground :: GroundTypeKind) polarity t1 tr.
           DolanSingularType ground polarity t1
        -> DolanPlainType ground polarity tr
        -> DolanPlainType ground polarity (JoinMeetType polarity t1 tr)

type DolanPlainShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanPlainShimWit ground polarity = PShimWit (DolanPolyShim ground Type) (DolanPlainType ground) polarity

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
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
        -> DolanSingularType ground polarity (UVar name)

type DolanSingularShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanSingularShimWit ground polarity = PShimWit (DolanPolyShim ground Type) (DolanSingularType ground) polarity

nilDolanPlainShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanPlainShimWit ground polarity (LimitType polarity)
nilDolanPlainShimWit = mkShimWit NilDolanPlainType

consDolanPlainShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) t1 tr.
       (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularShimWit ground polarity t1
    -> DolanPlainShimWit ground polarity tr
    -> DolanPlainShimWit ground polarity (JoinMeetType polarity t1 tr)
consDolanPlainShimWit (MkShimWit t1 conv1) (MkShimWit tr convr) =
    MkShimWit (ConsDolanPlainType t1 tr) (polarBimap conv1 convr)

unsafeDeleteVarPlainShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) name.
       (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanPlainShimWit ground polarity (UVar name)
unsafeDeleteVarPlainShimWit = MkShimWit NilDolanPlainType $ isoPolarBackwards unsafeUVarIsomorphism

class TypeFreeVariables (t :: Type) where
    typeFreeVariables :: t -> FiniteSet (AnyW SymbolType)

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             TypeFreeVariables (DolanType ground polarity t) where
    typeFreeVariables (PlainDolanType t) = typeFreeVariables t
    typeFreeVariables (RecursiveDolanType v t) = deleteSet (MkAnyW v) $ typeFreeVariables t

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             TypeFreeVariables (DolanPlainType ground polarity t) where
    typeFreeVariables NilDolanPlainType = mempty
    typeFreeVariables (ConsDolanPlainType t1 tr) = union (typeFreeVariables t1) (typeFreeVariables tr)

instance forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground =>
             TypeFreeVariables (DolanSingularType ground polarity t) where
    typeFreeVariables (GroundDolanSingularType gt args) = argumentsFreeVariables (groundTypeVarianceType gt) args
    typeFreeVariables (VarDolanSingularType v) = singletonSet $ MkAnyW v

argumentFreeVariables ::
       forall (ground :: GroundTypeKind) polarity v t. IsDolanGroundType ground
    => VarianceType v
    -> SingleArgument v (DolanType ground) polarity t
    -> FiniteSet (AnyW SymbolType)
argumentFreeVariables CovarianceType t = typeFreeVariables t
argumentFreeVariables ContravarianceType t = typeFreeVariables t
argumentFreeVariables RangevarianceType (MkRangeType p q) = union (typeFreeVariables p) (typeFreeVariables q)

argumentsFreeVariables ::
       forall (ground :: GroundTypeKind) polarity dv t ta. IsDolanGroundType ground
    => DolanVarianceType dv
    -> DolanArguments dv (DolanType ground) t polarity ta
    -> FiniteSet (AnyW SymbolType)
argumentsFreeVariables NilListType NilDolanArguments = mempty
argumentsFreeVariables (ConsListType sv dv) (ConsDolanArguments t1 tr) =
    union (argumentFreeVariables @ground @polarity sv t1) (argumentsFreeVariables dv tr)

dolanTypeToPlainNonrec ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => DolanType ground polarity t
    -> Maybe (DolanPlainType ground polarity t)
dolanTypeToPlainNonrec (PlainDolanType t) = Just t
dolanTypeToPlainNonrec (RecursiveDolanType n t)
    | notMember (MkAnyW n) (typeFreeVariables t) = Just t
dolanTypeToPlainNonrec _ = Nothing

singleDolanShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularShimWit ground polarity t
    -> DolanShimWit ground polarity t
singleDolanShimWit (MkShimWit st conv) = ccontramap conv $ MkShimWit (singleDolanType st) polar1

singleDolanPlainType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       DolanSingularType ground polarity t
    -> DolanPlainType ground polarity (JoinMeetType polarity t (LimitType polarity))
singleDolanPlainType st = ConsDolanPlainType st NilDolanPlainType

singleDolanPlainShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularShimWit ground polarity t
    -> DolanPlainShimWit ground polarity t
singleDolanPlainShimWit (MkShimWit st conv) = ccontramap conv $ MkShimWit (singleDolanPlainType st) polar1

singleDolanType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       DolanSingularType ground polarity t
    -> DolanType ground polarity (JoinMeetType polarity t (LimitType polarity))
singleDolanType st = PlainDolanType $ singleDolanPlainType st

plainDolanShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanPlainShimWit ground polarity t
    -> DolanShimWit ground polarity t
plainDolanShimWit = chainShimWit (\t -> mkShimWit $ PlainDolanType t)

plainRecursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) name (t :: Type).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanPlainShimWit ground polarity t
    -> DolanShimWit ground polarity t
plainRecursiveDolanShimWit n = chainShimWit (\t -> mkShimWit $ RecursiveDolanType n t)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Semigroup (AnyW (DolanPlainType ground polarity)) where
    MkAnyW NilDolanPlainType <> tb = tb
    MkAnyW (ConsDolanPlainType ta tr) <> tb =
        case MkAnyW tr <> tb of
            MkAnyW trb -> MkAnyW $ ConsDolanPlainType ta trb

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Monoid (AnyW (DolanPlainType ground polarity)) where
    mappend = (<>)
    mempty = MkAnyW NilDolanPlainType

class (IsDolanPolyShim (DolanPolyShim ground)) => IsDolanGroundType (ground :: GroundTypeKind) where
    groundTypeVarianceType ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). ground dv t -> DolanVarianceType dv
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). ground dv t -> DolanVarianceMap dv t
    groundTypeTestEquality ::
           forall (dva :: DolanVariance) (ta :: DolanVarianceKind dva) (dvb :: DolanVariance) (tb :: DolanVarianceKind dvb).
           ground dva ta
        -> ground dvb tb
        -> Maybe (dva :~: dvb, ta :~~: tb)

mapDolanGroundArguments ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => DolanType ground polarity' t' -> DolanShimWit ground polarity' t')
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> DolanSingularShimWit ground polarity t
mapDolanGroundArguments ff g args =
    case mapDolanArguments ff (groundTypeVarianceType g) (groundTypeVarianceMap g) args of
        MkShimWit args' conv -> MkShimWit (GroundDolanSingularType g args') conv

mapDolanSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => DolanType ground polarity' t' -> DolanShimWit ground polarity' t')
    -> DolanSingularType ground polarity t
    -> DolanSingularShimWit ground polarity t
mapDolanSingularType ff (GroundDolanSingularType gt args) = mapDolanGroundArguments ff gt args
mapDolanSingularType _ t@(VarDolanSingularType _) = mkShimWit t

mapDolanGroundArgumentsM ::
       forall m (ground :: GroundTypeKind) polarity dv gt t.
       (Monad m, IsDolanGroundType ground, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => DolanType ground polarity' t' -> m (DolanShimWit ground polarity' t'))
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> m (DolanSingularShimWit ground polarity t)
mapDolanGroundArgumentsM ff g args = do
    MkShimWit args' conv <- mapDolanArgumentsM ff (groundTypeVarianceType g) (groundTypeVarianceMap g) args
    return $ MkShimWit (GroundDolanSingularType g args') conv

mapDolanSingularTypeM ::
       forall m (ground :: GroundTypeKind) polarity t. (Monad m, IsDolanGroundType ground, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' => DolanType ground polarity' t' -> m (DolanShimWit ground polarity' t'))
    -> DolanSingularType ground polarity t
    -> m (DolanSingularShimWit ground polarity t)
mapDolanSingularTypeM ff (GroundDolanSingularType gt args) = mapDolanGroundArgumentsM ff gt args
mapDolanSingularTypeM _ t@(VarDolanSingularType _) = return $ mkShimWit t
