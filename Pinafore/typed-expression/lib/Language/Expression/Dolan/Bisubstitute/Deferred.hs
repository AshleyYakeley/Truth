module Language.Expression.Dolan.Bisubstitute.Deferred
    ( recursiveDolanShimWit
    , mapDolanSingularTypeM
    , Bisubstitutable
    , bisubstituteSingularType
    , bisubstituteType
    , bisubstituteGroundedType
    , funcBisubstituteType
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute.Bisubstitution
import Language.Expression.Dolan.Bisubstitute.RecM
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Shim
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type DeferredPolyShim :: PolyShimKind -> Type -> Type -> Type -> PolyShimKind
type DeferredPolyShim pshim oldtype newtypepos newtypeneg =
    PolyFuncShim
        ( PolarShim (pshim Type) 'Positive oldtype newtypepos
        , PolarShim (pshim Type) 'Negative oldtype newtypeneg
        )
        pshim

type DeferredShim :: PolyShimKind -> Type -> Type -> Type -> ShimKind Type
type DeferredShim pshim oldtype newtypepos newtypeneg = DeferredPolyShim pshim oldtype newtypepos newtypeneg Type

class Bisubstitutable (ground :: GroundTypeKind) (w :: Polarity -> Type -> Type) | w -> ground where
    recBisubstituteSingularType ::
        forall m (pshim :: PolyShimKind) (polarity :: Polarity) t.
        (MonadInner m, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
        Bisubstitution (DolanSingularType ground) (pshim Type) m ->
        w polarity t ->
        ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) w polarity t)
    recBisubstituteType ::
        forall m (pshim :: PolyShimKind) (polarity :: Polarity) t.
        (MonadInner m, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
        Bisubstitution (DolanType ground) (pshim Type) m ->
        w polarity t ->
        ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanType ground) polarity t)

instance
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    Bisubstitutable ground (DolanGroundedType ground)
    where
    recBisubstituteSingularType sub = mapDolanGroundedTypeM (recBisubstituteSingularType sub)
    recBisubstituteType sub = fmap shimWitToDolan . mapDolanGroundedTypeM (recBisubstituteType sub)

instance
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    Bisubstitutable ground (DolanSingularType ground)
    where
    recBisubstituteSingularType ::
        forall m (pshim :: PolyShimKind) (polarity :: Polarity) t.
        (MonadInner m, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
        Bisubstitution (DolanSingularType ground) (pshim Type) m ->
        DolanSingularType ground polarity t ->
        ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
    recBisubstituteSingularType sub@(MkBisubstitution nb mpos mneg) = \case
        VarDolanSingularType n' | Just Refl <- testEquality nb n' ->
            liftInner
                $ case polarityType @polarity of
                    PositiveType -> mpos
                    NegativeType -> mneg
        t@(RecursiveDolanSingularType nt _) | Just Refl <- testEquality nb nt -> return $ mkPolarShimWit t
        RecursiveDolanSingularType recvar pt -> do
            pts <- recBisubstituteSingularType sub pt
            lift $ recursiveDolanShimWitRecM recvar pts
        t -> mapDolanSingularTypeRecM (recBisubstituteSingularType sub) t

    recBisubstituteType ::
        forall m (pshim :: PolyShimKind) (polarity :: Polarity) t.
        (MonadInner m, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
        Bisubstitution (DolanType ground) (pshim Type) m ->
        DolanSingularType ground polarity t ->
        ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanType ground) polarity t)
    recBisubstituteType (MkBisubstitution n mpos mneg) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' =
            liftInner
                $ case polarityType @polarity of
                    PositiveType -> mpos
                    NegativeType -> mneg
    recBisubstituteType (MkBisubstitution nb _ _) t@(RecursiveDolanSingularType nt _)
        | Just Refl <- testEquality nb nt = return $ shimWitToDolan $ mkPolarShimWit t
    recBisubstituteType sub (RecursiveDolanSingularType recvar pt) = do
        pts <- recBisubstituteType sub pt
        stw <- lift $ recursiveDolanShimWitRecM recvar pts
        return $ shimWitToDolan stw
    recBisubstituteType sub t = do
        t' <- mapDolanSingularTypeRecM (recBisubstituteType sub) t
        return $ shimWitToDolan t'

instance
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    Bisubstitutable ground (DolanType ground)
    where
    recBisubstituteSingularType _ NilDolanType = return nilDolanShimWit
    recBisubstituteSingularType sub (ConsDolanType ta tb) = do
        tfa <- recBisubstituteSingularType sub ta
        tfb <- recBisubstituteSingularType sub tb
        return $ consDolanShimWit tfa tfb
    recBisubstituteType _ NilDolanType = return nilDolanShimWit
    recBisubstituteType sub (ConsDolanType ta tb) = do
        tfa <- recBisubstituteType sub ta
        tfb <- recBisubstituteType sub tb
        return $ joinMeetShimWit tfa tfb

recDeferBisubstituteType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) m oldtv newtypepos newtypeneg t.
    (Bisubstitutable ground w, MonadInner m, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    TypeVarT oldtv ->
    m (PShimWit (pshim Type) (DolanType ground) 'Positive newtypepos) ->
    m (PShimWit (pshim Type) (DolanType ground) 'Negative newtypeneg) ->
    w polarity t ->
    ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) polarity t)
recDeferBisubstituteType var mpos mneg wt = let
    mapPositive ::
        PShimWit (pshim Type) (DolanType ground) 'Positive newtypepos ->
        PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) 'Positive oldtv
    mapPositive (MkShimWit t (MkPolarShim conv)) =
        MkShimWit t $ MkPolarShim $ MkPolyMapT $ MkComposeShim $ \(MkPolarShim convp, _) -> conv . convp
    mapNegative ::
        PShimWit (pshim Type) (DolanType ground) 'Negative newtypeneg ->
        PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) 'Negative oldtv
    mapNegative (MkShimWit t (MkPolarShim conv)) =
        MkShimWit t $ MkPolarShim $ MkPolyMapT $ MkComposeShim $ \(_, MkPolarShim convn) -> convn . conv
    in recBisubstituteType (MkBisubstitution var (fmap mapPositive mpos) (fmap mapNegative mneg)) wt

deferBisubstituteType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) oldtv newtypepos newtypeneg t.
    (Bisubstitutable ground w, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    TypeVarT oldtv ->
    PShimWit (pshim Type) (DolanType ground) 'Positive newtypepos ->
    PShimWit (pshim Type) (DolanType ground) 'Negative newtypeneg ->
    w polarity t ->
    PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) polarity t
deferBisubstituteType var tpos tneg wt =
    runIdentity $ runRecM $ unComposeInner $ recDeferBisubstituteType var (pure tpos) (pure tneg) wt

funcBisubstituteType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) tv ta tb t r.
    (Is PolarityType polarity, Bisubstitutable ground w, SubstitutablePolyShim pshim) =>
    TypeVarT tv ->
    DolanType ground 'Positive ta ->
    DolanType ground 'Negative tb ->
    w polarity t ->
    ( forall t'.
      DolanType ground polarity t' -> (pshim Type tv ta -> pshim Type tb tv -> PolarShim (pshim Type) polarity t t') -> r
    ) ->
    r
funcBisubstituteType var tpos tneg wt call =
    case deferBisubstituteType @ground @pshim var (mkShimWit tpos) (mkShimWit tneg) wt of
        MkShimWit t conv ->
            call t $ \conva convb ->
                case polarityType @polarity of
                    PositiveType ->
                        MkPolarShim
                            $ (unComposeShim $ unPolyMapT $ unPolarShim conv) (MkPolarShim conva, MkPolarShim convb)
                    NegativeType ->
                        MkPolarShim
                            $ (unComposeShim $ unPolyMapT $ unPolarShim conv) (MkPolarShim conva, MkPolarShim convb)

reducePolarShim ::
    forall (pshim :: PolyShimKind) polarity a b c d.
    (ReduciblePolyShim pshim, Is PolarityType polarity) =>
    (PolarShim (ReducedPolyShim pshim Type) polarity a b -> PolarShim (ReducedPolyShim pshim Type) polarity c d) ->
    PolarShim (pshim Type) polarity a b ->
    PolarShim (pshim Type) polarity c d
reducePolarShim f (MkPolarShim conv) =
    MkPolarShim
        $ case polarityType @polarity of
            PositiveType -> reduceShim (unPolarShim . f . MkPolarShim) conv
            NegativeType -> reduceShim (unPolarShim . f . MkPolarShim) conv

runComposeIdentity :: Monad m => ComposeInner Identity m a -> m a
runComposeIdentity (MkComposeInner mia) = fmap runIdentity mia

recursiveBisubstitute ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv a.
    ( IsDolanGroundType ground
    , SubstitutablePolyShim pshim
    , LazyShim (pshim Type)
    , ReducedPolyShim pshim Type ~ pshim Type
    , Is PolarityType polarity
    ) =>
    TypeVarT tv ->
    DolanType ground polarity a ->
    RecM ground pshim (FuncShimWit (DolanSingularType ground polarity) (PolarShim (pshim Type) polarity) tv a)
recursiveBisubstitute oldvar t =
    memoiseRecM oldvar t
        $ withInvertPolarity @polarity
        $ newTypeVar (typeVarName oldvar)
        $ \(newvar :: TypeVarT newtv) ->
            case polarityType @polarity of
                PositiveType -> do
                    MkShimWit t' rconv <-
                        runComposeIdentity
                            $ recDeferBisubstituteType
                                @ground
                                @pshim
                                oldvar
                                (pure $ varDolanShimWit newvar)
                                (pure $ varDolanShimWit oldvar)
                                t
                    assignTypeVarWit newvar t'
                        $ return
                        $ MkFuncShimWit (RecursiveDolanSingularType newvar t')
                        $ \sconv -> let
                            conv = lazyPolarShim $ (applyPolarPolyFuncShim rconv (conv, id)) . sconv
                            in conv
                NegativeType -> do
                    MkShimWit t' rconv <-
                        runComposeIdentity
                            $ recDeferBisubstituteType
                                @ground
                                @pshim
                                oldvar
                                (pure $ varDolanShimWit oldvar)
                                (pure $ varDolanShimWit newvar)
                                t
                    assignTypeVarWit newvar t'
                        $ return
                        $ MkFuncShimWit (RecursiveDolanSingularType newvar t')
                        $ \sconv -> let
                            conv = lazyPolarShim $ (applyPolarPolyFuncShim rconv (id, conv)) . sconv
                            in conv

recursiveBisubstituteReduced ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv a.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    TypeVarT tv ->
    DolanType ground polarity a ->
    RecM ground (ReducedPolyShim pshim) (FuncShimWit (DolanSingularType ground polarity) (PolarShim (pshim Type) polarity) tv a)
recursiveBisubstituteReduced oldvar t =
    withDict (reducedSubstitutablePolyShim @pshim) $ do
        MkFuncShimWit t' conv <- recursiveBisubstitute @ground @(ReducedPolyShim pshim) oldvar t
        return $ MkFuncShimWit t' (reducePolarShim conv)

recursiveDolanShimWitRecM ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    TypeVarT tv ->
    PShimWit (pshim Type) (DolanType ground) polarity tv ->
    RecM ground (ReducedPolyShim pshim) (PShimWit (pshim Type) (DolanSingularType ground) polarity tv)
recursiveDolanShimWitRecM oldvar (MkShimWit t sconv) = do
    tw <- recursiveBisubstituteReduced oldvar t
    return $ mapFuncShimWit tw sconv

recursiveDolanShimWit ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    TypeVarT tv ->
    PShimWit (pshim Type) (DolanType ground) polarity tv ->
    PShimWit (pshim Type) (DolanSingularType ground) polarity tv
recursiveDolanShimWit oldvar tw = runRecM $ recursiveDolanShimWitRecM oldvar tw

mapDolanSingularTypeRecM ::
    forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
    (MonadInner m, IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    ( forall polarity' t'.
      Is PolarityType polarity' =>
      DolanType ground polarity' t' -> ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanType ground) polarity' t')
    ) ->
    DolanSingularType ground polarity t ->
    ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanSingularTypeRecM ff (GroundedDolanSingularType t) =
    fmap (chainShimWit $ \gt -> mkShimWit $ GroundedDolanSingularType gt) $ mapDolanGroundedTypeM ff t
mapDolanSingularTypeRecM _ t@(VarDolanSingularType _) = return $ mkPolarShimWit t
mapDolanSingularTypeRecM ff (RecursiveDolanSingularType var t) = do
    t' <- ff t
    lift $ recursiveDolanShimWitRecM var t'

mapDolanSingularTypeM ::
    forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
    (Monad m, IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity) =>
    ( forall polarity' t'.
      Is PolarityType polarity' =>
      DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t')
    ) ->
    DolanSingularType ground polarity t ->
    m (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanSingularTypeM ff (GroundedDolanSingularType t) =
    fmap (chainShimWit $ \gt -> mkShimWit $ GroundedDolanSingularType gt) $ mapDolanGroundedTypeM ff t
mapDolanSingularTypeM _ t@(VarDolanSingularType _) = return $ mkPolarShimWit t
mapDolanSingularTypeM ff (RecursiveDolanSingularType var t) = do
    t' <- ff t
    return $ recursiveDolanShimWit var t'

bisubstituteSingularType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) m t.
    (SubstitutablePolyShim pshim, Bisubstitutable ground w, Is PolarityType polarity, MonadInner m) =>
    Bisubstitution (DolanSingularType ground) (pshim Type) m ->
    w polarity t ->
    m (PShimWit (pshim Type) w polarity t)
bisubstituteSingularType bisub wt = runRecM $ unComposeInner $ recBisubstituteSingularType bisub wt

bisubstituteType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) m t.
    (SubstitutablePolyShim pshim, Bisubstitutable ground w, Is PolarityType polarity, MonadInner m) =>
    Bisubstitution (DolanType ground) (pshim Type) m ->
    w polarity t ->
    m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteType bisub wt = runRecM $ unComposeInner $ recBisubstituteType bisub wt

bisubstituteGroundedType ::
    forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) m t.
    (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity, MonadInner m) =>
    Bisubstitution (DolanType ground) (pshim Type) m ->
    DolanGroundedType ground polarity t ->
    m (PShimWit (pshim Type) (DolanGroundedType ground) polarity t)
bisubstituteGroundedType bisub wt = runRecM $ unComposeInner $ mapDolanGroundedTypeM (recBisubstituteType bisub) wt
