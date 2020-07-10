{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Combine
    ( IsJoinMeetWitness
    , joinMeetShimWit
    , joinMeetSemiIsoShimWit
    , recursiveMapType
    , recursiveDolanType
    , recursiveDolanShimWit
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.VarSubstitute
import Shapes

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
    joinMeetSemiIsoType ta@(RecursiveDolanType oldvara pta) tb@(PlainDolanType ptb) =
        invertPolarity @polarity $ let
            newname =
                runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- dolanNamespaceRename @ground ta
                        _ <- dolanNamespaceRename @ground tb
                        return ()
                    varRenamerTGenerate [uVarName oldvara]
            in newUVar newname $ \newvar -> let
                   sub :: VarSubstitution _ polarity _ _
                   sub = mkPolarVarSubstitution oldvara newvar
                   in case varSubstitute sub pta of
                          MkShimWit pta' rconv ->
                              case joinMeetSemiIsoType pta' ptb of
                                  MkShimWit ptab' jconv ->
                                      assignUVarWit newvar ptab' $ let
                                          conv =
                                              jconv <.>
                                              iPolarPair
                                                  (applyPolarPolyFuncShim rconv (lazyPolarSemiIso conv <.> polar1, cid))
                                                  cid
                                          in MkShimWit (RecursiveDolanType newvar ptab') conv
    joinMeetSemiIsoType ta@(PlainDolanType pta) tb@(RecursiveDolanType oldvarb ptb) =
        invertPolarity @polarity $ let
            newname =
                runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- dolanNamespaceRename @ground ta
                        _ <- dolanNamespaceRename @ground tb
                        return ()
                    varRenamerTGenerate [uVarName oldvarb]
            in newUVar newname $ \newvar -> let
                   sub :: VarSubstitution _ polarity _ _
                   sub = mkPolarVarSubstitution oldvarb newvar
                   in case varSubstitute sub ptb of
                          MkShimWit ptb' rconv ->
                              case joinMeetSemiIsoType pta ptb' of
                                  MkShimWit ptab' jconv ->
                                      assignUVarWit newvar ptab' $ let
                                          conv =
                                              jconv <.>
                                              iPolarPair
                                                  cid
                                                  (applyPolarPolyFuncShim rconv (lazyPolarSemiIso conv <.> polar2, cid))
                                          in MkShimWit (RecursiveDolanType newvar ptab') conv
    joinMeetSemiIsoType ta@(RecursiveDolanType oldvara pta) tb@(RecursiveDolanType oldvarb ptb) =
        invertPolarity @polarity $ let
            newname =
                runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- dolanNamespaceRename @ground ta
                        _ <- dolanNamespaceRename @ground tb
                        return ()
                    varRenamerTGenerate [uVarName oldvara, uVarName oldvarb]
            in newUVar newname $ \newvar -> let
                   suba :: VarSubstitution _ polarity _ _
                   suba = mkPolarVarSubstitution oldvara newvar
                   subb :: VarSubstitution _ polarity _ _
                   subb = mkPolarVarSubstitution oldvarb newvar
                   in case varSubstitute suba pta of
                          MkShimWit pta' rconva ->
                              case varSubstitute subb ptb of
                                  MkShimWit ptb' rconvb ->
                                      case joinMeetSemiIsoType pta' ptb' of
                                          MkShimWit ptab' jconv ->
                                              assignUVarWit newvar ptab' $ let
                                                  conv =
                                                      jconv .
                                                      iPolarPair
                                                          (applyPolarPolyFuncShim
                                                               rconva
                                                               (lazyPolarSemiIso conv . polar1, id))
                                                          (applyPolarPolyFuncShim
                                                               rconvb
                                                               (lazyPolarSemiIso conv . polar2, id))
                                                  in MkShimWit (RecursiveDolanType newvar ptab') conv

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

recursiveMapType ::
       forall (ground :: GroundTypeKind) polarity (name :: Symbol). (IsDolanGroundType ground, Is PolarityType polarity)
    => (forall a. DolanPlainType ground polarity a -> DolanSemiIsoPlainShimWit ground polarity a)
    -> SymbolType name
    -> DolanPlainType ground polarity (UVar Type name)
    -> DolanSemiIsoShimWit ground polarity (UVar Type name)
recursiveMapType f oldvar pt = let
    newname =
        runIdentity $
        runVarRenamerT $ do
            runVarNamespaceT $ do
                _ <- dolanNamespaceRename @ground pt
                return ()
            varRenamerTGenerate [uVarName oldvar]
    in newUVar newname $ \newvar ->
           invertPolarity @polarity $ let
               sub :: VarSubstitution _ polarity _ _
               sub = mkPolarVarSubstitution oldvar newvar
               in case varSubstitute sub pt of
                      MkShimWit pt' rconv ->
                          case f pt' of
                              MkShimWit tpt jconv ->
                                  assignUVarWit newvar tpt $ let
                                      conv = jconv . (applyPolarPolyFuncShim rconv (lazyPolarSemiIso conv, id))
                                      in MkShimWit (RecursiveDolanType newvar tpt) conv

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
        MkVarType newvar <-
            varNamespaceTAddNamesUVar @Type @_ @(DolanTypeSystem ground) [MkVarType var, MkVarType oldvar]
        pt' <- dolanNamespaceRename @ground pt
        return $ RecursiveDolanType newvar pt'

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> PolarMap (PolySemiIso (DolanPolyShim ground) Type) polarity (UVar Type name) t
    -> DolanSemiIsoShimWit ground polarity t
    -> DolanSemiIsoShimWit ground polarity t
recursiveDolanShimWit oldvar vconv (MkShimWit t sconv) =
    invertPolarity @polarity $
    newUVar (uVarName oldvar) $ \newvar ->
        case varSubstitute (mkPolarVarSubstitution @polarity oldvar newvar) t of
            MkShimWit t' rconv ->
                assignUVarWit newvar t' $ let
                    conv = (applyPolarPolyFuncShim rconv (lazyPolarSemiIso conv . vconv, id)) . sconv
                    in MkShimWit (recursiveDolanType newvar t') conv
