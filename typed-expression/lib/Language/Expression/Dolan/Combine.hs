{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Combine
    ( plainRecursiveDolanShimWit
    , IsJoinMeetWitness
    , joinMeetShimWit
    , recursiveDolanType
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
    joinMeetSemiIsoType ::
           forall polarity (a :: Type) (b :: Type). Is PolarityType polarity
        => w polarity a
        -> w polarity b
        -> PShimWit (PolySemiIso (JoinMeetShim w) Type) w polarity (JoinMeetType polarity a b)

joinMeetSemiIsoShimWit ::
       forall (w :: Polarity -> Type -> Type) (polarity :: Polarity) (a :: Type) (b :: Type).
       (IsJoinMeetWitness w, Is PolarityType polarity)
    => PShimWit (PolySemiIso (JoinMeetShim w) Type) w polarity a
    -> PShimWit (PolySemiIso (JoinMeetShim w) Type) w polarity b
    -> PShimWit (PolySemiIso (JoinMeetShim w) Type) w polarity (JoinMeetType polarity a b)
joinMeetSemiIsoShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    ccontramap (iPolarPair conva convb) $ joinMeetSemiIsoType ta tb

joinMeetType ::
       forall (w :: Polarity -> Type -> Type) polarity (a :: Type) (b :: Type).
       (IsJoinMeetWitness w, Is PolarityType polarity)
    => w polarity a
    -> w polarity b
    -> PShimWit (JoinMeetShim w Type) w polarity (JoinMeetType polarity a b)
joinMeetType ta tb =
    case joinMeetSemiIsoType ta tb of
        MkShimWit tab conv -> MkShimWit tab $ polarPolySemiIsoForwards conv

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
    joinMeetSemiIsoType NilDolanPlainType tb = MkShimWit tb iPolarL2
    joinMeetSemiIsoType (ConsDolanPlainType ta tr) tb =
        case joinMeetSemiIsoType tr tb of
            MkShimWit trb convrb -> MkShimWit (ConsDolanPlainType ta trb) $ iPolarPair cid convrb <.> iPolarSwapL

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => IsJoinMeetWitness (DolanType ground) where
    type JoinMeetShim (DolanType ground) = DolanPolyShim ground
    joinMeetSemiIsoType (PlainDolanType pta) (PlainDolanType ptb) =
        case joinMeetSemiIsoType pta ptb of
            MkShimWit ptab conv -> MkShimWit (PlainDolanType ptab) conv
    joinMeetSemiIsoType ta@(RecursiveDolanType na pta) tb@(PlainDolanType ptb) =
        runIdentity $
        runVarRenamerT $
        runVarNamespaceT $ do
            _ <- renameDolanType ta
            _ <- renameDolanType tb
            newname <- varNamespaceTAddNames [uVarName na]
            pta' <- renameDolanSemiIsoPlainType pta
            ptb' <- renameDolanSemiIsoPlainType ptb
            return $ plainRecursiveDolanShimWit newname $ joinMeetSemiIsoShimWit pta' ptb'
    joinMeetSemiIsoType ta@(PlainDolanType pta) tb@(RecursiveDolanType nb ptb) =
        runIdentity $
        runVarRenamerT $
        runVarNamespaceT $ do
            _ <- renameDolanType ta
            _ <- renameDolanType tb
            newname <- varNamespaceTAddNames [uVarName nb]
            pta' <- renameDolanSemiIsoPlainType pta
            ptb' <- renameDolanSemiIsoPlainType ptb
            return $ plainRecursiveDolanShimWit newname $ joinMeetSemiIsoShimWit pta' ptb'
    joinMeetSemiIsoType (RecursiveDolanType na pta) (RecursiveDolanType nb ptb)
        | Just Refl <- testEquality na nb =
            case joinMeetSemiIsoType pta ptb of
                MkShimWit ptab conv -> MkShimWit (plainRecursiveDolanType (uVarName na) ptab) conv
    joinMeetSemiIsoType rta@(RecursiveDolanType na ta) rtb@(RecursiveDolanType nb tb) =
        runIdentity $
        runVarRenamerT $
        runVarNamespaceT $ do
            _ <- renameDolanType rta
            _ <- renameDolanType rtb
            newname <- varNamespaceTAddNames [uVarName na, uVarName nb]
            ta' <- renameDolanSemiIsoPlainType ta
            tb' <- renameDolanSemiIsoPlainType tb
            return $ plainRecursiveDolanShimWit newname $ joinMeetSemiIsoShimWit ta' tb'

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
    -> DolanSemiIsoShimWit ground polarity t
recursiveDolanType n (PlainDolanType pt) = mkShimWit $ plainRecursiveDolanType n pt
recursiveDolanType nb (RecursiveDolanType na pt) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        newname <- varNamespaceTAddNames [uVarName na, nb]
        pt' <- renameDolanSemiIsoPlainType pt
        return $ plainRecursiveDolanShimWit newname pt'

recursiveDolanSemiIsoShimWit ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanSemiIsoShimWit ground polarity t
    -> DolanSemiIsoShimWit ground polarity t
recursiveDolanSemiIsoShimWit n = chainShimWit $ recursiveDolanType n

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanShimWit ground polarity t
    -> DolanShimWit ground polarity t
recursiveDolanShimWit n = chainShimWit $ \t -> polarPolySemiIsoShimWit $ recursiveDolanType n t

type SemiIsoSubstitution :: GroundTypeKind -> Type
data SemiIsoSubstitution ground =
    forall name (polarity :: Polarity). Is PolarityType polarity =>
                                            MkSemiIsoSubstitution (SymbolType name)
                                                                  (DolanSemiIsoShimWit ground polarity (UVar Type name))

class SemiIsoSubstitutable (ground :: GroundTypeKind) (polarity :: Polarity) (w :: Type -> Type)
    | w -> ground polarity
    where
    semiIsoSubstituteType :: forall t. SemiIsoSubstitution ground -> w t -> DolanSemiIsoShimWit ground polarity t

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) (shim :: ShimKind Type). ( SemiIsoSubstitutable ground polarity (w polarity)
         , Is PolarityType polarity
         , shim ~ DolanPolySemiIsoShim ground Type
         , InCategory shim
         ) => SemiIsoSubstitutable ground polarity (PShimWit shim w polarity) where
    semiIsoSubstituteType sub = chainShimWit $ semiIsoSubstituteType sub

substituteInVar ::
       forall (ground :: GroundTypeKind) polarity name. (IsDolanGroundType ground, Is PolarityType polarity)
    => SemiIsoSubstitution ground
    -> SymbolType name
    -> DolanSemiIsoShimWit ground polarity (UVar Type name)
substituteInVar (MkSemiIsoSubstitution sn (tw :: DolanSemiIsoShimWit ground polarity' _)) n
    | Just Refl <- testEquality sn n
    , Just Refl <- testEquality (polarityType @polarity) (polarityType @polarity') = tw
substituteInVar _ n = singleDolanShimWit $ mkShimWit $ VarDolanSingularType n

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             SemiIsoSubstitutable ground polarity (DolanSingularType ground polarity) where
    semiIsoSubstituteType sub (VarDolanSingularType n) = substituteInVar sub n
    semiIsoSubstituteType sub t = singleDolanShimWit $ mapDolanSingularType (semiIsoSubstituteType sub) t

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             SemiIsoSubstitutable ground polarity (DolanPlainType ground polarity) where
    semiIsoSubstituteType _ NilDolanPlainType = mkShimWit $ PlainDolanType NilDolanPlainType
    semiIsoSubstituteType sub (ConsDolanPlainType t1 tr) =
        joinMeetSemiIsoShimWit (semiIsoSubstituteType sub t1) (semiIsoSubstituteType sub tr)

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             SemiIsoSubstitutable ground polarity (DolanType ground polarity) where
    semiIsoSubstituteType sub (PlainDolanType pt) = semiIsoSubstituteType sub pt
    semiIsoSubstituteType (MkSemiIsoSubstitution sn _) t@(RecursiveDolanType n _)
        | Just Refl <- testEquality sn n = mkShimWit t
    semiIsoSubstituteType sub (RecursiveDolanType n pt) =
        recursiveDolanSemiIsoShimWit (uVarName n) (semiIsoSubstituteType sub pt)

unrollSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SemiIsoSubstitution ground
    -> DolanSingularType ground polarity t
    -> DolanSemiIsoPlainShimWit ground polarity t
unrollSingularType (MkSemiIsoSubstitution n (_ :: DolanSemiIsoShimWit ground polarity' _)) (VarDolanSingularType n')
    | Just Refl <- testEquality n n'
    , Just Refl <- testEquality (polarityType @polarity) (polarityType @polarity') = unsafeDeleteVarPlainShimWit n
unrollSingularType sub t = singleDolanPlainShimWit $ mapDolanSingularType (semiIsoSubstituteType sub) t

unrollPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SemiIsoSubstitution ground
    -> DolanPlainType ground polarity t
    -> DolanSemiIsoPlainShimWit ground polarity t
unrollPlainType _ NilDolanPlainType = mkShimWit NilDolanPlainType
unrollPlainType sub (ConsDolanPlainType t1 tr) =
    joinMeetSemiIsoShimWit (unrollSingularType sub t1) (unrollPlainType sub tr)

dolanTypeToPlainUnroll ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanSemiIsoPlainShimWit ground polarity t
dolanTypeToPlainUnroll (PlainDolanType pt) = mkShimWit pt
dolanTypeToPlainUnroll t@(RecursiveDolanType n pt) = unrollPlainType (MkSemiIsoSubstitution n $ mkShimWit t) pt
