{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Combine
    ( joinMeetDolanPlainShimWit
    , joinMeetDolanShimWit
    , recursiveDolanType
    , recursiveDolanShimWit
    , dolanTypeToPlainRotate
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeVariable
import Shapes

joinMeetDolanPlainTypes ::
       forall (ground :: GroundTypeKind) polarity (a :: Type) (b :: Type).
       (JoinMeetCategory (DolanPolyShim ground Type), Is PolarityType polarity)
    => DolanPlainType ground polarity a
    -> DolanPlainType ground polarity b
    -> DolanPlainShimWit ground polarity (JoinMeetType polarity a b)
joinMeetDolanPlainTypes NilDolanPlainType tb = MkShimWit tb $ polarF polarLimit cid
joinMeetDolanPlainTypes (ConsDolanPlainType ta tr) tb =
    case joinMeetDolanPlainTypes tr tb of
        MkShimWit trb convrb ->
            MkShimWit (ConsDolanPlainType ta trb) $
            polarF (polarF polar1 (polar2 <.> convrb <.> polar1)) (polar2 <.> convrb <.> polar2)

joinMeetDolanPlainShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetCategory (DolanPolyShim ground Type), Is PolarityType polarity)
    => DolanPlainShimWit ground polarity a
    -> DolanPlainShimWit ground polarity b
    -> DolanPlainShimWit ground polarity (JoinMeetType polarity a b)
joinMeetDolanPlainShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    ccontramap (polarBimap conva convb) $ joinMeetDolanPlainTypes ta tb

joinMeetDolanTypes ::
       forall (ground :: GroundTypeKind) polarity (a :: Type) (b :: Type).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> DolanType ground polarity b
    -> DolanShimWit ground polarity (JoinMeetType polarity a b)
joinMeetDolanTypes (PlainDolanType pta) (PlainDolanType ptb) =
    case joinMeetDolanPlainTypes pta ptb of
        MkShimWit ptab conv -> MkShimWit (PlainDolanType ptab) conv
joinMeetDolanTypes rta@(RecursiveDolanType na ta) rtb@(PlainDolanType tb) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        _ <- renameDolanType rta
        _ <- renameDolanType rtb
        varNamespaceTAddUVars @_ @_ @(DolanPolyShim ground Type) (ConsListType na NilListType) $ \nab _ -> do
            ta' <- renameDolanPlainType ta
            tb' <- renameDolanPlainType tb
            return $ plainRecursiveDolanShimWit nab $ joinMeetDolanPlainShimWit ta' tb'
joinMeetDolanTypes rta@(PlainDolanType ta) rtb@(RecursiveDolanType nb tb) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        _ <- renameDolanType rta
        _ <- renameDolanType rtb
        varNamespaceTAddUVars @_ @_ @(DolanPolyShim ground Type) (ConsListType nb NilListType) $ \nab _ -> do
            ta' <- renameDolanPlainType ta
            tb' <- renameDolanPlainType tb
            return $ plainRecursiveDolanShimWit nab $ joinMeetDolanPlainShimWit ta' tb'
joinMeetDolanTypes (RecursiveDolanType na pta) (RecursiveDolanType nb ptb)
    | Just Refl <- testEquality na nb =
        case joinMeetDolanPlainTypes pta ptb of
            MkShimWit ptab conv -> MkShimWit (RecursiveDolanType na ptab) conv
joinMeetDolanTypes rta@(RecursiveDolanType na ta) rtb@(RecursiveDolanType nb tb) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        _ <- renameDolanType rta
        _ <- renameDolanType rtb
        varNamespaceTAddUVars @_ @_ @(DolanPolyShim ground Type) (ConsListType na $ ConsListType nb NilListType) $ \nab _ -> do
            ta' <- renameDolanPlainType ta
            tb' <- renameDolanPlainType tb
            return $ plainRecursiveDolanShimWit nab $ joinMeetDolanPlainShimWit ta' tb'

joinMeetDolanShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanShimWit ground polarity a
    -> DolanShimWit ground polarity b
    -> DolanShimWit ground polarity (JoinMeetType polarity a b)
joinMeetDolanShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    ccontramap (polarBimap conva convb) $ joinMeetDolanTypes ta tb

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Semigroup (AnyW (DolanType ground polarity)) where
    MkAnyW ta <> MkAnyW tb = shimWitToAnyW $ joinMeetDolanTypes ta tb

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
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity t
    -> DolanShimWit ground polarity t
recursiveDolanType n (PlainDolanType pt) = mkShimWit $ RecursiveDolanType n pt
recursiveDolanType nb (RecursiveDolanType na pt) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        varNamespaceTAddUVars @_ @_ @(DolanPolyShim ground Type) (ConsListType nb $ ConsListType na NilListType) $ \nab _ -> do
            pt' <- renameDolanPlainType pt
            return $ plainRecursiveDolanShimWit nab pt'

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanShimWit ground polarity t
    -> DolanShimWit ground polarity t
recursiveDolanShimWit n = chainShimWit $ recursiveDolanType n

type Sub :: GroundTypeKind -> Type
data Sub ground =
    forall name (polarity :: Polarity). Is PolarityType polarity =>
                                            MkSub (SymbolType name)
                                                  (DolanShimWit ground polarity (UVar name))

substituteInVar ::
       forall (ground :: GroundTypeKind) polarity name. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> SymbolType name
    -> DolanShimWit ground polarity (UVar name)
substituteInVar (MkSub sn (tw :: DolanShimWit ground polarity' _)) n
    | Just Refl <- testEquality sn n
    , Just Refl <- testEquality (polarityType @polarity) (polarityType @polarity') = tw
substituteInVar _ n = singleDolanShimWit $ mkShimWit $ VarDolanSingularType n

substituteInSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanSingularType ground polarity t
    -> DolanShimWit ground polarity t
substituteInSingularType sub (VarDolanSingularType n) = substituteInVar sub n
substituteInSingularType sub t = singleDolanShimWit $ mapDolanSingularType (substituteInType sub) t

substituteInPlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanPlainType ground polarity t
    -> DolanShimWit ground polarity t
substituteInPlainType _ NilDolanPlainType = mkShimWit $ PlainDolanType NilDolanPlainType
substituteInPlainType sub (ConsDolanPlainType t1 tr) =
    joinMeetDolanShimWit (substituteInSingularType sub t1) (substituteInPlainType sub tr)

substituteInType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanType ground polarity t
    -> DolanShimWit ground polarity t
substituteInType sub (PlainDolanType pt) = substituteInPlainType sub pt
substituteInType (MkSub sn _) t@(RecursiveDolanType n _)
    | Just Refl <- testEquality sn n = mkShimWit t
substituteInType sub (RecursiveDolanType n pt) = recursiveDolanShimWit n (substituteInPlainType sub pt)

rotateSingularType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanSingularType ground polarity t
    -> DolanPlainShimWit ground polarity t
rotateSingularType (MkSub n (_ :: DolanShimWit ground polarity' _)) (VarDolanSingularType n')
    | Just Refl <- testEquality n n'
    , Just Refl <- testEquality (polarityType @polarity) (polarityType @polarity') = unsafeDeleteVarPlainShimWit
rotateSingularType sub t = singleDolanPlainShimWit $ mapDolanSingularType (substituteInType sub) t

rotatePlainType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => Sub ground
    -> DolanPlainType ground polarity t
    -> DolanPlainShimWit ground polarity t
rotatePlainType _ NilDolanPlainType = mkShimWit NilDolanPlainType
rotatePlainType sub (ConsDolanPlainType t1 tr) =
    joinMeetDolanPlainShimWit (rotateSingularType sub t1) (rotatePlainType sub tr)

dolanTypeToPlainRotate ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> DolanPlainShimWit ground polarity t
dolanTypeToPlainRotate (PlainDolanType pt) = mkShimWit pt
dolanTypeToPlainRotate t@(RecursiveDolanType n pt) =
    rotatePlainType (MkSub n $ MkShimWit t $ isoBackwards unsafeUVarIsomorphism) pt
