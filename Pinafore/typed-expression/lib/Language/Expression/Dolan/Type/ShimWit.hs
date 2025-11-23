module Language.Expression.Dolan.Type.ShimWit where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Shim
import Language.Expression.Dolan.Type.DolanType
import Language.Expression.Dolan.Type.Equality ()
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type DolanTypeSub :: GroundTypeKind -> Polarity -> (Type -> Type) -> Constraint
class Is PolarityType polarity => DolanTypeSub ground polarity w | w -> ground polarity where
    typeToDolan ::
        forall (shim :: ShimKind Type) t.
        JoinMeetIsoShim shim =>
        w t ->
        PShimWit shim (DolanType ground) polarity t
    dolanToMaybeType ::
        forall (shim :: ShimKind Type) t.
        JoinMeetIsoShim shim =>
        DolanType ground polarity t ->
        Maybe (PolarShimWit shim w polarity t)

dolanToMaybeTypeShim ::
    forall (ground :: GroundTypeKind) polarity w t.
    (DolanTypeSub ground polarity w, JoinMeetIsoShim (DolanShim ground)) =>
    DolanType ground polarity t ->
    Maybe (PolarShimWit (DolanShim ground) w polarity t)
dolanToMaybeTypeShim = dolanToMaybeType

typeToSomeDolan ::
    forall (ground :: GroundTypeKind) polarity w t.
    (DolanTypeSub ground polarity w, JoinMeetIsoShim (DolanShim ground)) =>
    w t ->
    Some (DolanType ground polarity)
typeToSomeDolan t = shimWitToSome $ typeToDolan @ground @polarity @w @(DolanShim ground) t

shimWitToDolan ::
    forall (ground :: GroundTypeKind) polarity w (shim :: ShimKind Type) t.
    (DolanTypeSub ground polarity w, JoinMeetIsoShim shim) =>
    PolarShimWit shim w polarity t ->
    PShimWit shim (DolanType ground) polarity t
shimWitToDolan (MkShimWit wt conv) = mapShimWit conv $ typeToDolan wt

dolanToMaybeShimWit ::
    forall (ground :: GroundTypeKind) polarity w (shim :: ShimKind Type) t.
    (DolanTypeSub ground polarity w, JoinMeetIsoShim shim) =>
    PShimWit shim (DolanType ground) polarity t ->
    Maybe (PolarShimWit shim w polarity t)
dolanToMaybeShimWit (MkShimWit wt conv) = fmap (mapShimWit conv) $ dolanToMaybeType wt

typeToDolanPositive ::
    forall (ground :: GroundTypeKind) polarity w t.
    (IsDolanGroundType ground, DolanTypeSub ground polarity w) =>
    w t ->
    ShimWit (DolanShim ground) (DolanType ground polarity) t
typeToDolanPositive wt =
    case polarityType @polarity of
        PositiveType -> switchShimWit unPolarShim $ typeToDolan wt
        NegativeType -> switchShimWit (unCatDual . unPolarShim) $ typeToDolan wt

typeToDolanNegative ::
    forall (ground :: GroundTypeKind) polarity w t.
    (IsDolanGroundType ground, DolanTypeSub ground polarity w) =>
    w t ->
    ShimWit (CatDual (DolanShim ground)) (DolanType ground polarity) t
typeToDolanNegative wt =
    case polarityType @polarity of
        PositiveType -> switchShimWit unPolarShim $ typeToDolan wt
        NegativeType -> switchShimWit (MkCatDual . unPolarShim) $ typeToDolan wt

instance
    forall (ground :: GroundTypeKind) polarity.
    Is PolarityType polarity =>
    DolanTypeSub ground polarity (DolanPartialGroundedType ground '[] polarity)
    where
    typeToDolan pgt = typeToDolan $ partialToGroundedType pgt NilCCRArguments
    dolanToMaybeType t = do
        gt <- dolanToMaybeType t
        pure $ reShimWit groundedToPartialType gt

instance
    forall (ground :: GroundTypeKind) polarity.
    Is PolarityType polarity =>
    DolanTypeSub ground polarity (DolanGroundedType ground polarity)
    where
    typeToDolan t = typeToDolan $ GroundedDolanSingularType t
    dolanToMaybeType t = do
        MkShimWit st conv <- dolanToMaybeType t
        case st of
            GroundedDolanSingularType gt -> return $ MkShimWit gt conv
            _ -> Nothing

instance
    forall (ground :: GroundTypeKind) polarity.
    Is PolarityType polarity =>
    DolanTypeSub ground polarity (DolanSingularType ground polarity)
    where
    typeToDolan t = MkShimWit (singleDolanType t) iPolarR1
    dolanToMaybeType (ConsDolanType t NilDolanType) = Just $ MkShimWit t iPolarL1
    dolanToMaybeType _ = Nothing

instance
    forall (ground :: GroundTypeKind) polarity.
    Is PolarityType polarity =>
    DolanTypeSub ground polarity (DolanType ground polarity)
    where
    typeToDolan = mkShimWit
    dolanToMaybeType = Just . mkShimWit

type DolanSingularShimWit :: GroundTypeKind -> Polarity -> Type -> Type
type DolanSingularShimWit ground polarity = PShimWit (DolanShim ground) (DolanSingularType ground) polarity

varDolanShimWit ::
    forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) t.
    (JoinMeetIsoShim shim, Is PolarityType polarity) =>
    TypeVarT t ->
    PShimWit shim (DolanType ground) polarity t
varDolanShimWit var = typeToDolan $ VarDolanSingularType var

nilDolanShimWit ::
    forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity).
    (Category shim, Is PolarityType polarity) =>
    PShimWit shim (DolanType ground) polarity (LimitType polarity)
nilDolanShimWit = mkPolarShimWit NilDolanType

consDolanShimWit ::
    forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) t1 tr.
    (JoinMeetIsoShim shim, Is PolarityType polarity) =>
    PShimWit shim (DolanSingularType ground) polarity t1 ->
    PShimWit shim (DolanType ground) polarity tr ->
    PShimWit shim (DolanType ground) polarity (JoinMeetType polarity t1 tr)
consDolanShimWit (MkShimWit t1 conv1) (MkShimWit tr convr) = MkShimWit (ConsDolanType t1 tr) (iPolarPair conv1 convr)

unsafeDeleteVarShimWit ::
    forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) t.
    (JoinMeetIsoShim shim, Is PolarityType polarity) =>
    TypeVarT t ->
    PShimWit shim (DolanType ground) polarity t
unsafeDeleteVarShimWit n = assignTypeVarT @(LimitType polarity) n nilDolanShimWit

singleDolanType ::
    forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
    DolanSingularType ground polarity t ->
    DolanType ground polarity (JoinMeetType polarity t (LimitType polarity))
singleDolanType st = ConsDolanType st NilDolanType

mapDolanGroundedTypeM ::
    forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
    (Monad m, IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    ( forall polarity' t'.
      Is PolarityType polarity' =>
      DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t')
    ) ->
    DolanGroundedType ground polarity t ->
    m (PShimWit (pshim Type) (DolanGroundedType ground) polarity t)
mapDolanGroundedTypeM ff (MkDolanGroundedType g args) = do
    args' <- mapDolanArgumentsM ff (groundTypeVarianceMap g) args
    return $ mkDolanGroundedShimWit g args'
