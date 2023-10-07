module Language.Expression.Dolan.Bisubstitute.Deferred
    ( recursiveDolanShimWit
    , mapDolanSingularTypeM
    , bisubstituteType
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

type DeferredShim :: PolyShimKind -> Type -> Type -> Type -> ShimKind Type
type DeferredShim pshim oldtype newtypepos newtypeneg
     = PolyFuncShim ( PolarShim (pshim Type) 'Positive oldtype newtypepos
                    , PolarShim (pshim Type) 'Negative oldtype newtypeneg) pshim Type

type DeferredBisubstitution :: (Type -> Type) -> GroundTypeKind -> ShimKind Type -> Type -> Type -> Type -> Type
data DeferredBisubstitution m ground shim tv newtypepos newtypeneg where
    MkDeferredBisubstitution
        :: forall m (ground :: GroundTypeKind) (shim :: ShimKind Type) tv newtypepos newtypeneg.
           TypeVarT tv
        -> m (PShimWit shim (DolanType ground) 'Positive newtypepos)
        -> m (PShimWit shim (DolanType ground) 'Negative newtypeneg)
        -> DeferredBisubstitution m ground shim tv newtypepos newtypeneg

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m oldname newtypepos newtypeneg. ( IsDolanGroundType ground
         , Traversable m
         ) => VarRenameable (DeferredBisubstitution m ground shim oldname newtypepos newtypeneg) where
    varRename ev =
        MkEndoM $ \(MkDeferredBisubstitution var mpos mneg) -> do
            mpos' <- unEndoM (endoFor $ varRename ev) mpos
            mneg' <- unEndoM (endoFor $ varRename ev) mneg
            return $ MkDeferredBisubstitution var mpos' mneg'

class Bisubstitutable (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type)
    | w -> ground polarity
    where
    deferBisubstituteType ::
           forall m oldtv newtypepos newtypeneg t. MonadInner m
        => DeferredBisubstitution m ground (pshim Type) oldtv newtypepos newtypeneg
        -> w t
        -> ComposeInner m (RecM ground (ReducedPolyShim pshim)) (PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) polarity t)

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , SubstitutablePolyShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanSingularType ground polarity) where
    deferBisubstituteType (MkDeferredBisubstitution n mpos mneg) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' =
            liftInner $
            case polarityType @polarity of
                PositiveType -> do
                    MkShimWit t conv <- mpos
                    return $ MkShimWit t $ mkPolarPolyFuncShim $ \(convpos, _) -> conv . convpos
                NegativeType -> do
                    MkShimWit t conv <- mneg
                    return $ MkShimWit t $ mkPolarPolyFuncShim $ \(_, convneg) -> conv . convneg
    deferBisubstituteType (MkDeferredBisubstitution nb _ _) t@(RecursiveDolanSingularType nt _)
        | Just Refl <- testEquality nb nt = return $ shimWitToDolan $ mkPolarShimWit t
    deferBisubstituteType sub (RecursiveDolanSingularType recvar pt) = do
        pts <- deferBisubstituteType sub pt
        stw <- lift $ recursiveDolanShimWitRecM recvar pts
        return $ shimWitToDolan stw
    deferBisubstituteType sub t = do
        t' <- mapDolanSingularTypeRecM (deferBisubstituteType sub) t
        return $ shimWitToDolan t'

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , SubstitutablePolyShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanType ground polarity) where
    deferBisubstituteType _ NilDolanType = return nilDolanShimWit
    deferBisubstituteType sub (ConsDolanType ta tb) = do
        tfa <- deferBisubstituteType sub ta
        tfb <- deferBisubstituteType sub tb
        return $ joinMeetShimWit tfa tfb

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
            PositiveType -> let
                dbisub :: DeferredBisubstitution Identity ground (ReducedPolyShim pshim Type) tv newtv tv
                dbisub = MkDeferredBisubstitution oldvar (pure $ varDolanShimWit newvar) (pure $ varDolanShimWit oldvar)
                in do
                       MkShimWit t' rconv <- runComposeIdentity $ deferBisubstituteType dbisub t
                       assignTypeVarWit newvar t' $
                           return $
                           MkFuncShimWit (RecursiveDolanSingularType newvar t') $ \sconv -> let
                               conv = lazyPolarShim $ (applyPolarPolyFuncShim rconv (conv, id)) . sconv
                               in conv
            NegativeType -> let
                dbisub :: DeferredBisubstitution Identity ground (ReducedPolyShim pshim Type) tv tv newtv
                dbisub = MkDeferredBisubstitution oldvar (pure $ varDolanShimWit oldvar) (pure $ varDolanShimWit newvar)
                in do
                       MkShimWit t' rconv <- runComposeIdentity $ deferBisubstituteType dbisub t
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

mapDolanGroundedTypeM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (Monad m, IsDolanGroundType ground, SubstitutablePolyShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> DolanGroundedType ground polarity t
    -> m (PShimWit (pshim Type) (DolanGroundedType ground) polarity t)
mapDolanGroundedTypeM ff (MkDolanGroundedType g args) = do
    MkShimWit args' conv <- mapDolanArgumentsM ff (groundTypeVarianceMap g) args
    return $ MkShimWit (MkDolanGroundedType g args') conv

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

deferredBisubstitution ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m r.
       Bisubstitution ground shim m
    -> (forall tv. DeferredBisubstitution m ground shim tv tv tv -> r)
    -> r
deferredBisubstitution (MkBisubstitution var mpos mneg) call = call $ MkDeferredBisubstitution var mpos mneg

bisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) m t.
       (SubstitutablePolyShim pshim, Bisubstitutable ground pshim polarity w, Is PolarityType polarity, MonadInner m)
    => Bisubstitution ground (pshim Type) m
    -> w t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteType bisub wt =
    deferredBisubstitution bisub $ \dbisub -> do
        MkShimWit wt' fconv <- runRecM $ unComposeInner $ deferBisubstituteType dbisub wt
        return $ MkShimWit wt' $ applyPolarPolyFuncShim fconv (id, id)
