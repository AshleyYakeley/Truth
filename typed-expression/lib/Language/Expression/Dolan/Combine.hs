{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Combine
    ( plainRecursiveDolanShimWitWRONG
    , IsJoinMeetWitness
    , joinMeetShimWit
    , recursiveDolanTypeWRONG
    , recursiveDolanShimWitWRONG
    , dolanTypeToPlainUnroll
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

plainRecursiveDolanShimWitWRONG ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) (polarity :: Polarity) (t :: Type).
       (InCategory shim, IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> PShimWit shim (DolanPlainType ground) polarity t
    -> PShimWit shim (DolanType ground) polarity t
plainRecursiveDolanShimWitWRONG n = chainShimWit (\t -> mkShimWit $ plainRecursiveDolanTypeWRONG n t)

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
joinMeetSemiIsoShimWit = chainPShimWit2 joinMeetSemiIsoType

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

lazyPolarSemiIso ::
       forall (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (EnhancedFunction (pshim Type), Is PolarityType polarity)
    => PolarMap (PolySemiIso pshim Type) polarity a b
    -> PolarMap (PolySemiIso pshim Type) polarity a b
lazyPolarSemiIso (MkPolarMap mab) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType ->
            case mab of
                MkPolyMapT ~(MkSemiIsomorphism ab mba) ->
                    MkPolyMapT $ MkSemiIsomorphism (lazyEnhanced ab) (fmap lazyEnhanced mba)
        NegativeType ->
            case mab of
                MkPolyMapT ~(MkSemiIsomorphism ab mba) ->
                    MkPolyMapT $ MkSemiIsomorphism (lazyEnhanced ab) (fmap lazyEnhanced mba)

recursiveMapType ::
       forall (ground :: GroundTypeKind) polarity (name :: Symbol). (IsDolanGroundType ground, Is PolarityType polarity)
    => (forall a. DolanType ground polarity a -> DolanSemiIsoShimWit ground polarity a)
    -> SymbolType name
    -> String
    -> DolanPlainType ground polarity (UVar Type name)
    -> DolanSemiIsoShimWit ground polarity (UVar Type name)
recursiveMapType f oldvar newname pt =
    newUVar newname $ \newvar ->
        invertPolarity @polarity $ let
            sub :: Sub ground _ polarity _ _
            sub =
                MkSub
                    oldvar
                    (singleDolanType $ VarDolanSingularType newvar)
                    (singleDolanType $ VarDolanSingularType oldvar)
            in case substituteType sub pt of
                   MkShimWit pt' rconv ->
                       case f pt' of
                           MkShimWit (PlainDolanType tpt) jconv ->
                               assignUVarWit newvar tpt $ let
                                   conv =
                                       jconv .
                                       (applyPolarPolyFuncShim rconv (iPolarR1 . lazyPolarSemiIso conv, iPolarR1))
                                   in MkShimWit (RecursiveDolanType newvar tpt) conv
                           MkShimWit (RecursiveDolanType _ _) _ -> error "recursive type in transform"

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => IsJoinMeetWitness (DolanType ground) where
    type JoinMeetShim (DolanType ground) = DolanPolyShim ground
    joinMeetSemiIsoType ::
           forall polarity (a :: Type) (b :: Type). Is PolarityType polarity
        => DolanType ground polarity a
        -> DolanType ground polarity b
        -> DolanSemiIsoShimWit ground polarity (JoinMeetType polarity a b)
    joinMeetSemiIsoType (PlainDolanType pta) (PlainDolanType ptb) =
        case joinMeetSemiIsoType pta ptb of
            MkShimWit ptab conv -> MkShimWit (PlainDolanType ptab) conv
    joinMeetSemiIsoType ta@(RecursiveDolanType oldvara pta) tb@(PlainDolanType _) =
        invertPolarity @polarity $ let
            newname =
                runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- renameDolanType ta
                        _ <- renameDolanType tb
                        return ()
                    varRenamerTGenerate [uVarName oldvara]
            in newUVar newname $ \newvar -> let
                   sub :: Sub ground _ polarity _ _
                   sub =
                       MkSub
                           oldvara
                           (singleDolanType $ VarDolanSingularType newvar)
                           (singleDolanType $ VarDolanSingularType oldvara)
                   in case substituteType sub pta of
                          MkShimWit ta' rconv ->
                              case joinMeetSemiIsoType ta' tb of
                                  MkShimWit (PlainDolanType ptab') jconv ->
                                      assignUVarWit newvar ptab' $ let
                                          conv =
                                              jconv <.>
                                              iPolarPair
                                                  (applyPolarPolyFuncShim
                                                       rconv
                                                       (iPolarR1 <.> lazyPolarSemiIso conv <.> polar1, iPolarR1))
                                                  cid
                                          in MkShimWit (RecursiveDolanType newvar ptab') conv
                                  MkShimWit (RecursiveDolanType _ _) _ -> error "recursive combine"
    joinMeetSemiIsoType ta@(PlainDolanType _) tb@(RecursiveDolanType oldvarb ptb) =
        invertPolarity @polarity $ let
            newname =
                runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- renameDolanType ta
                        _ <- renameDolanType tb
                        return ()
                    varRenamerTGenerate [uVarName oldvarb]
            in newUVar newname $ \newvar -> let
                   sub :: Sub ground _ polarity _ _
                   sub =
                       MkSub
                           oldvarb
                           (singleDolanType $ VarDolanSingularType newvar)
                           (singleDolanType $ VarDolanSingularType oldvarb)
                   in case substituteType sub ptb of
                          MkShimWit tb' rconv ->
                              case joinMeetSemiIsoType ta tb' of
                                  MkShimWit (PlainDolanType ptab') jconv ->
                                      assignUVarWit newvar ptab' $ let
                                          conv =
                                              jconv <.>
                                              iPolarPair
                                                  cid
                                                  (applyPolarPolyFuncShim
                                                       rconv
                                                       (iPolarR1 <.> lazyPolarSemiIso conv <.> polar2, iPolarR1))
                                          in MkShimWit (RecursiveDolanType newvar ptab') conv
                                  MkShimWit (RecursiveDolanType _ _) _ -> error "recursive combine"
    joinMeetSemiIsoType (RecursiveDolanType na pta) (RecursiveDolanType nb ptb) -- TODO
        | Just Refl <- testEquality na nb =
            case joinMeetSemiIsoType pta ptb of
                MkShimWit ptab conv -> MkShimWit (plainRecursiveDolanTypeWRONG (uVarName na) ptab) conv
    joinMeetSemiIsoType rta@(RecursiveDolanType na ta) rtb@(RecursiveDolanType nb tb) -- TODO
     =
        runIdentity $
        runVarRenamerT $
        runVarNamespaceT $ do
            _ <- renameDolanType rta
            _ <- renameDolanType rtb
            newname <- varNamespaceTAddNames [uVarName na, uVarName nb]
            ta' <- renameDolanPlainType ta
            tb' <- renameDolanPlainType tb
            return $ plainRecursiveDolanShimWitWRONG newname $ joinMeetSemiIsoType ta' tb'

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

recursiveDolanTypeWRONG ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanType ground polarity t
    -> DolanSemiIsoShimWit ground polarity t
recursiveDolanTypeWRONG n (PlainDolanType pt) = mkShimWit $ plainRecursiveDolanTypeWRONG n pt
recursiveDolanTypeWRONG nb (RecursiveDolanType na pt) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        newname <- varNamespaceTAddNames [uVarName na, nb]
        pt' <- renameDolanPlainType pt
        return $ mkShimWit $ plainRecursiveDolanTypeWRONG newname pt'

recursiveDolanSemiIsoShimWitWRONG ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanSemiIsoShimWit ground polarity t
    -> DolanSemiIsoShimWit ground polarity t
recursiveDolanSemiIsoShimWitWRONG n = chainShimWit $ recursiveDolanTypeWRONG n

recursiveDolanShimWitWRONG ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => String
    -> DolanShimWit ground polarity t
    -> DolanShimWit ground polarity t
recursiveDolanShimWitWRONG n = chainShimWit $ \t -> polarPolySemiIsoShimWit $ recursiveDolanTypeWRONG n t

recursiveDolanType ::
       forall (ground :: GroundTypeKind) polarity name. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity (UVar Type name)
    -> DolanType ground polarity (UVar Type name)
recursiveDolanType var (PlainDolanType pt) = RecursiveDolanType var pt
recursiveDolanType var (RecursiveDolanType oldvar pt) =
    runIdentity $
    runVarRenamerT $
    runVarNamespaceT $ do
        newname <- varNamespaceTAddNames [uVarName var, uVarName oldvar]
        newUVar newname $ \newvar -> do
            pt' <- renameDolanPlainType pt
            assignUVarWit newvar pt' $ return $ RecursiveDolanType newvar pt'

---
type SubShim :: GroundTypeKind -> Symbol -> Polarity -> Type -> Type -> ShimKind Type
type SubShim ground name polarity tp tn
     = PolyFuncShim ( PolarMap (DolanPolySemiIsoShim ground Type) polarity (UVar Type name) tp
                    , PolarMap (DolanPolySemiIsoShim ground Type) (InvertPolarity polarity) (UVar Type name) tn) (DolanPolySemiIsoShim ground) Type

type Sub :: GroundTypeKind -> Symbol -> Polarity -> Type -> Type -> Type
data Sub ground name polarity tp tn =
    MkSub (SymbolType name)
          (DolanType ground polarity tp)
          (DolanType ground (InvertPolarity polarity) tn)

class Substitutable (ground :: GroundTypeKind) (polarity :: Polarity) (w :: Type -> Type) | w -> ground polarity where
    substituteType ::
           forall name polarity' tp tn t. Is PolarityType polarity'
        => Sub ground name polarity' tp tn
        -> w t
        -> PShimWit (SubShim ground name polarity' tp tn) (DolanType ground) polarity t

substituteInVar ::
       forall (ground :: GroundTypeKind) polarity polarity' name tp tn name'.
       (IsDolanGroundType ground, Is PolarityType polarity, Is PolarityType polarity')
    => Sub ground name polarity' tp tn
    -> SymbolType name'
    -> PShimWit (SubShim ground name polarity' tp tn) (DolanType ground) polarity (UVar Type name')
substituteInVar (MkSub var tp tn) var'
    | Just Refl <- testEquality var var' =
        case samePolarity @polarity @polarity' of
            Left Refl -> MkShimWit tp $ mkPolarPolyFuncShim $ \(convp, _) -> convp
            Right Refl -> MkShimWit tn $ mkPolarPolyFuncShim $ \(_, convn) -> convn
substituteInVar _ var' =
    case singleDolanShimWit $ mkShimWit $ VarDolanSingularType var' of
        MkShimWit t conv -> MkShimWit t $ mkPolarPolyFuncShim $ \_ -> conv

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Substitutable ground polarity (DolanSingularType ground polarity) where
    substituteType sub (VarDolanSingularType var') = substituteInVar sub var'
    substituteType sub t = singleDolanShimWit $ mapDolanSingularType (substituteType sub) t

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Substitutable ground polarity (DolanPlainType ground polarity) where
    substituteType _ NilDolanPlainType = mkShimWit $ PlainDolanType NilDolanPlainType
    substituteType sub (ConsDolanPlainType t1 tr) =
        chainPShimWit2
            (\ta tb -> purePolyComposeShimWit $ joinMeetSemiIsoType ta tb)
            (substituteType sub t1)
            (substituteType sub tr)

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Substitutable ground polarity (DolanType ground polarity) where
    substituteType sub (PlainDolanType pt) = substituteType sub pt
    substituteType (MkSub sn _ _) t@(RecursiveDolanType n _)
        | Just Refl <- testEquality sn n = mkShimWit t
    substituteType sub (RecursiveDolanType var pt) =
        recursiveMapType (\t -> foo substituteType sub t) var (uVarName var) pt

{-
        newUVar (uVarName oldvar) $ \newvar -> case substituteType sub pt of
            MkShimWit t' conv -> assignUVarWit newvar t' $ -- WRONG
                MkShimWit (recursiveDolanType newvar t') conv


recursiveMapType :: forall (ground :: GroundTypeKind) polarity (name :: Symbol). (IsDolanGroundType ground, Is PolarityType polarity) =>
    (forall a. DolanType ground polarity a -> DolanSemiIsoShimWit ground polarity a) ->
    SymbolType name ->
    String ->
    DolanPlainType ground polarity (UVar Type name) -> DolanSemiIsoShimWit ground polarity (UVar Type name)
-}
---
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

semiIsoSubstituteInVar ::
       forall (ground :: GroundTypeKind) polarity name. (IsDolanGroundType ground, Is PolarityType polarity)
    => SemiIsoSubstitution ground
    -> SymbolType name
    -> DolanSemiIsoShimWit ground polarity (UVar Type name)
semiIsoSubstituteInVar (MkSemiIsoSubstitution sn (tw :: DolanSemiIsoShimWit ground polarity' _)) n
    | Just Refl <- testEquality sn n
    , Just Refl <- testEquality (polarityType @polarity) (polarityType @polarity') = tw
semiIsoSubstituteInVar _ n = singleDolanShimWit $ mkShimWit $ VarDolanSingularType n

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             SemiIsoSubstitutable ground polarity (DolanSingularType ground polarity) where
    semiIsoSubstituteType sub (VarDolanSingularType n) = semiIsoSubstituteInVar sub n
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
        recursiveDolanSemiIsoShimWitWRONG (uVarName n) (semiIsoSubstituteType sub pt)

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
