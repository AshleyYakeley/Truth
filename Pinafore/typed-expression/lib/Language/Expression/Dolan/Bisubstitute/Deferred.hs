module Language.Expression.Dolan.Bisubstitute.Deferred
    ( recursiveDolanShimWit
    , mapDolanSingularTypeM
    , bisubstituteType
    , funcBisubstituteType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute.Bisubstitution
import Language.Expression.Dolan.Bisubstitute.RecM
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Shim
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type DeferredPolyShim :: PolyShimKind -> Type -> Type -> Type -> PolyShimKind
type DeferredPolyShim pshim oldtype newtypepos newtypeneg
     = PolyFuncShim ( PolarShim (pshim Type) 'Positive oldtype newtypepos
                    , PolarShim (pshim Type) 'Negative oldtype newtypeneg) pshim

type DeferredShim :: PolyShimKind -> Type -> Type -> Type -> ShimKind Type
type DeferredShim pshim oldtype newtypepos newtypeneg = DeferredPolyShim pshim oldtype newtypepos newtypeneg Type

class Bisubstitutable (ground :: GroundTypeKind) (polarity :: Polarity) (w :: Type -> Type) | w -> ground polarity where
    recBisubstituteType ::
           forall m (pshim :: PolyShimKind) t. (MonadInner m, SubstitutablePolyShim pshim)
        => Bisubstitution ground (pshim Type) m
        -> w t
        -> ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanType ground) polarity t)

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanSingularType ground polarity) where
    recBisubstituteType (MkBisubstitution n mpos mneg) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' =
            liftInner $
            case polarityType @polarity of
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

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanType ground polarity) where
    recBisubstituteType _ NilDolanType = return nilDolanShimWit
    recBisubstituteType sub (ConsDolanType ta tb) = do
        tfa <- recBisubstituteType sub ta
        tfb <- recBisubstituteType sub tb
        return $ joinMeetShimWit tfa tfb

recDeferBisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) m oldtv newtypepos newtypeneg t.
       (Bisubstitutable ground polarity w, MonadInner m, SubstitutablePolyShim pshim)
    => TypeVarT oldtv
    -> m (PShimWit (pshim Type) (DolanType ground) 'Positive newtypepos)
    -> m (PShimWit (pshim Type) (DolanType ground) 'Negative newtypeneg)
    -> w t
    -> ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) polarity t)
recDeferBisubstituteType var mpos mneg wt = let
    mapPositive ::
           PShimWit (pshim Type) (DolanType ground) 'Positive newtypepos
        -> PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) 'Positive oldtv
    mapPositive (MkShimWit t (MkPolarShim conv)) =
        MkShimWit t $ MkPolarShim $ MkPolyMapT $ MkComposeShim $ \(MkPolarShim convp, _) -> conv . convp
    mapNegative ::
           PShimWit (pshim Type) (DolanType ground) 'Negative newtypeneg
        -> PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) 'Negative oldtv
    mapNegative (MkShimWit t (MkPolarShim conv)) =
        MkShimWit t $ MkPolarShim $ MkPolyMapT $ MkComposeShim $ \(_, MkPolarShim convn) -> convn . conv
    in recBisubstituteType (MkBisubstitution var (fmap mapPositive mpos) (fmap mapNegative mneg)) wt

deferBisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) oldtv newtypepos newtypeneg t.
       (Bisubstitutable ground polarity w, SubstitutablePolyShim pshim)
    => TypeVarT oldtv
    -> PShimWit (pshim Type) (DolanType ground) 'Positive newtypepos
    -> PShimWit (pshim Type) (DolanType ground) 'Negative newtypeneg
    -> w t
    -> PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) polarity t
deferBisubstituteType var tpos tneg wt =
    runIdentity $ runRecM $ unComposeInner $ recDeferBisubstituteType var (pure tpos) (pure tneg) wt

funcBisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) tv ta tb t r.
       (Is PolarityType polarity, Bisubstitutable ground polarity w, SubstitutablePolyShim pshim)
    => TypeVarT tv
    -> DolanType ground 'Positive ta
    -> DolanType ground 'Negative tb
    -> w t
    -> (forall t'.
                DolanType ground polarity t' -> (pshim Type tv ta -> pshim Type tb tv -> PolarShim (pshim Type) polarity t t') -> r)
    -> r
funcBisubstituteType var tpos tneg wt call =
    case deferBisubstituteType @ground @pshim var (mkShimWit tpos) (mkShimWit tneg) wt of
        MkShimWit t conv ->
            call t $ \conva convb ->
                case polarityType @polarity of
                    PositiveType ->
                        MkPolarShim $
                        (unComposeShim $ unPolyMapT $ unPolarShim conv) (MkPolarShim conva, MkPolarShim convb)
                    NegativeType ->
                        MkPolarShim $
                        (unComposeShim $ unPolyMapT $ unPolarShim conv) (MkPolarShim conva, MkPolarShim convb)

reducePolarShim ::
       forall (pshim :: PolyShimKind) polarity a b c d. (ReduciblePolyShim pshim, Is PolarityType polarity)
    => (PolarShim (ReducedPolyShim pshim Type) polarity a b -> PolarShim (ReducedPolyShim pshim Type) polarity c d)
    -> PolarShim (pshim Type) polarity a b
    -> PolarShim (pshim Type) polarity c d
reducePolarShim f (MkPolarShim conv) =
    MkPolarShim $
    case polarityType @polarity of
        PositiveType -> reduceShim (unPolarShim . f . MkPolarShim) conv
        NegativeType -> reduceShim (unPolarShim . f . MkPolarShim) conv

runComposeIdentity :: Monad m => ComposeInner Identity m a -> m a
runComposeIdentity (MkComposeInner mia) = fmap runIdentity mia

recursiveBisubstitute ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv a.
       ( IsDolanGroundType ground
       , SubstitutablePolyShim pshim
       , LazyCategory (pshim Type)
       , ReducedPolyShim pshim Type ~ pshim Type
       , Is PolarityType polarity
       )
    => TypeVarT tv
    -> DolanType ground polarity a
    -> RecM ground pshim (FuncShimWit (DolanSingularType ground polarity) (PolarShim (pshim Type) polarity) tv a)
recursiveBisubstitute oldvar t =
    memoiseRecM oldvar t $
    withInvertPolarity @polarity $
    newTypeVar (typeVarName oldvar) $ \(newvar :: TypeVarT newtv) ->
        case polarityType @polarity of
            PositiveType -> do
                MkShimWit t' rconv <-
                    runComposeIdentity $
                    recDeferBisubstituteType
                        @ground
                        @pshim
                        oldvar
                        (pure $ varDolanShimWit newvar)
                        (pure $ varDolanShimWit oldvar)
                        t
                assignTypeVarWit newvar t' $
                    return $
                    MkFuncShimWit (RecursiveDolanSingularType newvar t') $ \sconv -> let
                        conv = lazyPolarShim $ (applyPolarPolyFuncShim rconv (conv, id)) . sconv
                        in conv
            NegativeType -> do
                MkShimWit t' rconv <-
                    runComposeIdentity $
                    recDeferBisubstituteType
                        @ground
                        @pshim
                        oldvar
                        (pure $ varDolanShimWit oldvar)
                        (pure $ varDolanShimWit newvar)
                        t
                assignTypeVarWit newvar t' $
                    return $
                    MkFuncShimWit (RecursiveDolanSingularType newvar t') $ \sconv -> let
                        conv = lazyPolarShim $ (applyPolarPolyFuncShim rconv (id, conv)) . sconv
                        in conv

recursiveBisubstituteReduced ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv a.
       (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity a
    -> RecM ground (ReducedPolyShim pshim) (FuncShimWit (DolanSingularType ground polarity) (PolarShim (pshim Type) polarity) tv a)
recursiveBisubstituteReduced oldvar t =
    withDict (reducedSubstitutablePolyShim @pshim) $ do
        MkFuncShimWit t' conv <- recursiveBisubstitute @ground @(ReducedPolyShim pshim) oldvar t
        return $ MkFuncShimWit t' (reducePolarShim conv)

recursiveDolanShimWitRecM ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv.
       (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => TypeVarT tv
    -> PShimWit (pshim Type) (DolanType ground) polarity tv
    -> RecM ground (ReducedPolyShim pshim) (PShimWit (pshim Type) (DolanSingularType ground) polarity tv)
recursiveDolanShimWitRecM oldvar (MkShimWit t sconv) = do
    tw <- recursiveBisubstituteReduced oldvar t
    return $ mapFuncShimWit tw sconv

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv.
       (IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => TypeVarT tv
    -> PShimWit (pshim Type) (DolanType ground) polarity tv
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity tv
recursiveDolanShimWit oldvar tw = runRecM $ recursiveDolanShimWitRecM oldvar tw

mapDolanSingularTypeRecM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (MonadInner m, IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> DolanSingularType ground polarity t
    -> ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanSingularTypeRecM ff (GroundedDolanSingularType t) =
    fmap (chainShimWit $ \gt -> mkShimWit $ GroundedDolanSingularType gt) $ mapDolanGroundedTypeM ff t
mapDolanSingularTypeRecM _ t@(VarDolanSingularType _) = return $ mkPolarShimWit t
mapDolanSingularTypeRecM ff (RecursiveDolanSingularType var t) = do
    t' <- ff t
    lift $ recursiveDolanShimWitRecM var t'

mapDolanSingularTypeM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (Monad m, IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> DolanSingularType ground polarity t
    -> m (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanSingularTypeM ff (GroundedDolanSingularType t) =
    fmap (chainShimWit $ \gt -> mkShimWit $ GroundedDolanSingularType gt) $ mapDolanGroundedTypeM ff t
mapDolanSingularTypeM _ t@(VarDolanSingularType _) = return $ mkPolarShimWit t
mapDolanSingularTypeM ff (RecursiveDolanSingularType var t) = do
    t' <- ff t
    return $ recursiveDolanShimWit var t'

bisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) m t.
       (SubstitutablePolyShim pshim, Bisubstitutable ground polarity w, Is PolarityType polarity, MonadInner m)
    => Bisubstitution ground (pshim Type) m
    -> w t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteType bisub wt = runRecM $ unComposeInner $ recBisubstituteType bisub wt
