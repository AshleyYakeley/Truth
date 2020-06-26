{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Combine
    ( plainRecursiveDolanShimWit
    , IsJoinMeetWitness
    , joinMeetShimWit
    , recursiveDolanType
    , recursiveDolanIsoShimWit
    , recursiveDolanShimWit
    , dolanTypeToPlainUnroll
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeVariable
import Shapes

plainRecursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) (t :: Type).
       (InCategory shim, IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> PShimWit shim (DolanPlainType ground) polarity t
    -> PShimWit shim (DolanType ground) polarity t
plainRecursiveDolanShimWit n = chainShimWit (\t -> mkShimWit $ plainRecursiveDolanType n t)

class JoinMeetCategory (JoinMeetShim w Type) => IsJoinMeetWitness (w :: Polarity -> Type -> Type) where
    type JoinMeetShim w :: PolyShimKind
    joinMeetIsoType ::
           forall polarity (a :: Type) (b :: Type). Is PolarityType polarity
        => w polarity a
        -> w polarity b
        -> PShimWit (PolyIso (JoinMeetShim w) Type) w polarity (JoinMeetType polarity a b)

joinMeetIsoShimWit ::
       forall (w :: Polarity -> Type -> Type) (polarity :: Polarity) (a :: Type) (b :: Type).
       (IsJoinMeetWitness w, Is PolarityType polarity)
    => PShimWit (PolyIso (JoinMeetShim w) Type) w polarity a
    -> PShimWit (PolyIso (JoinMeetShim w) Type) w polarity b
    -> PShimWit (PolyIso (JoinMeetShim w) Type) w polarity (JoinMeetType polarity a b)
joinMeetIsoShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    ccontramap (iPolarPair conva convb) $ joinMeetIsoType ta tb

joinMeetType ::
       forall (w :: Polarity -> Type -> Type) polarity (a :: Type) (b :: Type).
       (IsJoinMeetWitness w, Is PolarityType polarity)
    => w polarity a
    -> w polarity b
    -> PShimWit (JoinMeetShim w Type) w polarity (JoinMeetType polarity a b)
joinMeetType ta tb =
    case joinMeetIsoType ta tb of
        MkShimWit tab conv -> MkShimWit tab $ polarPolyIsoForwards conv

joinMeetShimWit ::
       forall (w :: Polarity -> Type -> Type) (polarity :: Polarity) (a :: Type) (b :: Type).
       (IsJoinMeetWitness w, Is PolarityType polarity)
    => PShimWit (JoinMeetShim w Type) w polarity a
    -> PShimWit (JoinMeetShim w Type) w polarity b
    -> PShimWit (JoinMeetShim w Type) w polarity (JoinMeetType polarity a b)
joinMeetShimWit (MkShimWit ta conva) (MkShimWit tb convb) = ccontramap (iPolarPair conva convb) $ joinMeetType ta tb

instance forall (ground :: GroundTypeKind). JoinMeetCategory (DolanPolyShim ground Type) =>
             IsJoinMeetWitness (DolanPlainType ground) where
    type JoinMeetShim (DolanPlainType ground) = DolanPolyShim ground
    joinMeetIsoType NilDolanPlainType tb = MkShimWit tb iPolarL2
    joinMeetIsoType (ConsDolanPlainType ta tr) tb =
        case joinMeetIsoType tr tb of
            MkShimWit trb convrb -> MkShimWit (ConsDolanPlainType ta trb) $ iPolarPair cid convrb <.> iPolarSwapL

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => IsJoinMeetWitness (DolanType ground) where
    type JoinMeetShim (DolanType ground) = DolanPolyShim ground
    joinMeetIsoType (PlainDolanType pta) (PlainDolanType ptb) =
        case joinMeetIsoType pta ptb of
            MkShimWit ptab conv -> MkShimWit (PlainDolanType ptab) conv
    joinMeetIsoType rta@(RecursiveDolanType na ta) rtb@(PlainDolanType tb) =
        runIdentity $
        runVarRenamerT $
        runVarNamespaceT $ do
            _ <- renameDolanType rta
            _ <- renameDolanType rtb
            newname <- varNamespaceTAddNames [uVarName na]
            ta' <- renameDolanIsoPlainType ta
            tb' <- renameDolanIsoPlainType tb
            return $ plainRecursiveDolanShimWit newname $ joinMeetIsoShimWit ta' tb'
    joinMeetIsoType rta@(PlainDolanType ta) rtb@(RecursiveDolanType nb tb) =
        runIdentity $
        runVarRenamerT $
        runVarNamespaceT $ do
            _ <- renameDolanType rta
            _ <- renameDolanType rtb
            newname <- varNamespaceTAddNames [uVarName nb]
            ta' <- renameDolanIsoPlainType ta
            tb' <- renameDolanIsoPlainType tb
            return $ plainRecursiveDolanShimWit newname $ joinMeetIsoShimWit ta' tb'
    joinMeetIsoType (RecursiveDolanType na pta) (RecursiveDolanType nb ptb)
        | Just Refl <- testEquality na nb =
            case joinMeetIsoType pta ptb of
                MkShimWit ptab conv -> MkShimWit (plainRecursiveDolanType (uVarName na) ptab) conv
    joinMeetIsoType rta@(RecursiveDolanType na ta) rtb@(RecursiveDolanType nb tb) =
        runIdentity $
        runVarRenamerT $
        runVarNamespaceT $ do
            _ <- renameDolanType rta
            _ <- renameDolanType rtb
            newname <- varNamespaceTAddNames [uVarName na, uVarName nb]
            ta' <- renameDolanIsoPlainType ta
            tb' <- renameDolanIsoPlainType tb
            return $ plainRecursiveDolanShimWit newname $ joinMeetIsoShimWit ta' tb'

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Semigroup (AnyW (DolanType ground polarity)) where
    MkAnyW ta <> MkAnyW tb = shimWitToAnyW $ joinMeetType ta tb

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Monoid (AnyW (DolanType ground polarity)) where
    mappend = (<>)
    mempty = MkAnyW $ PlainDolanType NilDolanPlainType

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Semigroup (AnyInKind (RangeType (DolanType ground) polarity)) where
    MkAnyInKind (MkRangeType tp1 tq1) <> MkAnyInKind (MkRangeType tp2 tq2) =
        invertPolarity @polarity $
        case (MkAnyW tp1 <> MkAnyW tp2, MkAnyW tq1 <> MkAnyW tq2) of
            (MkAnyW tp12, MkAnyW tq12) -> MkAnyInKind (MkRangeType tp12 tq12)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Monoid (AnyInKind (RangeType (DolanType ground) polarity)) where
    mappend = (<>)
    mempty = MkAnyInKind (MkRangeType (PlainDolanType NilDolanPlainType) (PlainDolanType NilDolanPlainType))

recursiveDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanType ground polarity t
    -> DolanIsoShimWit ground polarity t
recursiveDolanType n (PlainDolanType pt) = mkShimWit $ plainRecursiveDolanType n pt
recursiveDolanType nb (RecursiveDolanType na pt) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        newname <- varNamespaceTAddNames [uVarName na, nb]
        pt' <- renameDolanIsoPlainType pt
        return $ plainRecursiveDolanShimWit newname pt'

recursiveDolanIsoShimWit ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanIsoShimWit ground polarity t
    -> DolanIsoShimWit ground polarity t
recursiveDolanIsoShimWit n = chainShimWit $ recursiveDolanType n

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanShimWit ground polarity t
    -> DolanShimWit ground polarity t
recursiveDolanShimWit n = chainShimWit $ \t -> polarPolyIsoShimWit $ recursiveDolanType n t

type Sub :: GroundTypeKind -> Type
data Sub ground =
    forall name (polarity :: Polarity). Is PolarityType polarity =>
                                            MkSub (SymbolType name)
                                                  (DolanIsoShimWit ground polarity (UVar Type name))

substituteInVar ::
       forall (ground :: GroundTypeKind) polarity name. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> SymbolType name
    -> DolanIsoShimWit ground polarity (UVar Type name)
substituteInVar (MkSub sn (tw :: DolanIsoShimWit ground polarity' _)) n
    | Just Refl <- testEquality sn n
    , Just Refl <- testEquality (polarityType @polarity) (polarityType @polarity') = tw
substituteInVar _ n = singleDolanShimWit $ mkShimWit $ VarDolanSingularType n

substituteInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanSingularType ground polarity t
    -> DolanIsoShimWit ground polarity t
substituteInSingularType sub (VarDolanSingularType n) = substituteInVar sub n
substituteInSingularType sub t = singleDolanShimWit $ mapDolanSingularType (substituteInType sub) t

substituteInPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanPlainType ground polarity t
    -> DolanIsoShimWit ground polarity t
substituteInPlainType _ NilDolanPlainType = mkShimWit $ PlainDolanType NilDolanPlainType
substituteInPlainType sub (ConsDolanPlainType t1 tr) =
    joinMeetIsoShimWit (substituteInSingularType sub t1) (substituteInPlainType sub tr)

substituteInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanType ground polarity t
    -> DolanIsoShimWit ground polarity t
substituteInType sub (PlainDolanType pt) = substituteInPlainType sub pt
substituteInType (MkSub sn _) t@(RecursiveDolanType n _)
    | Just Refl <- testEquality sn n = mkShimWit t
substituteInType sub (RecursiveDolanType n pt) = recursiveDolanIsoShimWit (uVarName n) (substituteInPlainType sub pt)

unrollSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanSingularType ground polarity t
    -> DolanIsoPlainShimWit ground polarity t
unrollSingularType (MkSub n (_ :: DolanIsoShimWit ground polarity' _)) (VarDolanSingularType n')
    | Just Refl <- testEquality n n'
    , Just Refl <- testEquality (polarityType @polarity) (polarityType @polarity') = unsafeDeleteVarPlainShimWit n
unrollSingularType sub t = singleDolanPlainShimWit $ mapDolanSingularType (substituteInType sub) t

unrollPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanPlainType ground polarity t
    -> DolanIsoPlainShimWit ground polarity t
unrollPlainType _ NilDolanPlainType = mkShimWit NilDolanPlainType
unrollPlainType sub (ConsDolanPlainType t1 tr) = joinMeetIsoShimWit (unrollSingularType sub t1) (unrollPlainType sub tr)

dolanTypeToPlainUnroll ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanIsoPlainShimWit ground polarity t
dolanTypeToPlainUnroll (PlainDolanType pt) = mkShimWit pt
dolanTypeToPlainUnroll t@(RecursiveDolanType n pt) = unrollPlainType (MkSub n $ mkShimWit t) pt
