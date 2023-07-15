{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Bisubstitute
    ( DeferredBisubstitution(..)
    , Bisubstitution(..)
    , mkPolarBisubstitution
    , mkSingleBisubstitution
    , Bisubstitutable(..)
    , bisubstituteType
    , singleBisubstitute
    , bisubstitutesType
    , bisubstitute
    , bisubstitutes
    , BisubstitutablePolyShim
    , recursiveDolanShimWit
    , recursiveRenameDolanShimWit
    , mapDolanSingularType
    , mapDolanSingularTypeM
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Rename ()
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type DeferredShim :: PolyShimKind -> Type -> Type -> Type -> ShimKind Type
type DeferredShim pshim oldtype newtypepos newtypeneg
     = PolyFuncShim ( PolarMap (pshim Type) 'Positive oldtype newtypepos
                    , PolarMap (pshim Type) 'Negative oldtype newtypeneg) pshim Type

type DeferredBisubstitution :: (Type -> Type) -> GroundTypeKind -> ShimKind Type -> Type -> Type -> Type -> Type
data DeferredBisubstitution m ground shim tv newtypepos newtypeneg where
    MkDeferredBisubstitution
        :: forall m (ground :: GroundTypeKind) (shim :: ShimKind Type) tv newtypepos newtypeneg.
           Bool
        -> TypeVarT tv
        -> m (PShimWit shim (DolanType ground) 'Positive newtypepos)
        -> m (PShimWit shim (DolanType ground) 'Negative newtypeneg)
        -> DeferredBisubstitution m ground shim tv newtypepos newtypeneg

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m oldname newtypepos newtypeneg. ( IsDolanGroundType ground
         , Traversable m
         ) => VarRenameable (DeferredBisubstitution m ground shim oldname newtypepos newtypeneg) where
    varRename ev =
        MkEndoM $ \(MkDeferredBisubstitution isRecursive var mpos mneg) -> do
            mpos' <- unEndoM (endoFor $ varRename ev) mpos
            mneg' <- unEndoM (endoFor $ varRename ev) mneg
            return $ MkDeferredBisubstitution isRecursive var mpos' mneg'

type Bisubstitution :: GroundTypeKind -> ShimKind Type -> (Type -> Type) -> Type
data Bisubstitution ground shim m =
    forall tv. MkBisubstitution Bool
                                (TypeVarT tv)
                                (m (PShimWit shim (DolanType ground) 'Positive tv))
                                (m (PShimWit shim (DolanType ground) 'Negative tv))

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m. ( MonadInner m
         , AllConstraint Show (DolanType ground 'Positive)
         , AllConstraint Show (DolanType ground 'Negative)
         ) => Show (Bisubstitution ground shim m) where
    show (MkBisubstitution isRecursive var mtpos mtneg) = let
        srec =
            if isRecursive
                then " (recursive)"
                else ""
        spos =
            case mToMaybe mtpos of
                Just (MkShimWit t _) -> allShow t
                Nothing -> "FAILS"
        sneg =
            case mToMaybe mtneg of
                Just (MkShimWit t _) -> allShow t
                Nothing -> "FAILS"
        in show var <> srec <> " => " <> "{+ => " <> spos <> "; - => " <> sneg <> "}"

deferredBisubstitution ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m r.
       Bisubstitution ground shim m
    -> (forall tv. DeferredBisubstitution m ground shim tv tv tv -> r)
    -> r
deferredBisubstitution (MkBisubstitution isRecursive var mpos mneg) call =
    call $ MkDeferredBisubstitution isRecursive var mpos mneg

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m. (IsDolanGroundType ground, Traversable m) =>
             VarRenameable (Bisubstitution ground shim m) where
    varRename ev =
        MkEndoM $ \(MkBisubstitution isRecursive var mpos mneg) -> do
            mpos' <- unEndoM (endoFor $ varRename ev) mpos
            mneg' <- unEndoM (endoFor $ varRename ev) mneg
            return $ MkBisubstitution isRecursive var mpos' mneg'

mkPolarBisubstitution ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) polarity m tv. Is PolarityType polarity
    => Bool
    -> TypeVarT tv
    -> m (PShimWit shim (DolanType ground) polarity tv)
    -> m (PShimWit shim (DolanType ground) (InvertPolarity polarity) tv)
    -> Bisubstitution ground shim m
mkPolarBisubstitution isRecursive n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution isRecursive n a b
        NegativeType -> MkBisubstitution isRecursive n b a

mkSingleBisubstitution ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) polarity m tv.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity, Applicative m)
    => Bool
    -> TypeVarT tv
    -> m (PShimWit shim (DolanType ground) polarity tv)
    -> Bisubstitution ground shim m
mkSingleBisubstitution isRecursive var mw =
    invertPolarity @polarity $ mkPolarBisubstitution isRecursive var mw $ pure $ varDolanShimWit var

class Bisubstitutable (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type)
    | w -> ground polarity
    where
    deferBisubstituteType ::
           forall m oldtv newtypepos newtypeneg t. MonadInner m
        => DeferredBisubstitution m ground (pshim Type) oldtv newtypepos newtypeneg
        -> w t
        -> m (PShimWit (DeferredShim pshim oldtv newtypepos newtypeneg) (DolanType ground) polarity t)

bisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) m t.
       (BisubstitutablePolyShim pshim, Bisubstitutable ground pshim polarity w, Is PolarityType polarity, MonadInner m)
    => Bisubstitution ground (pshim Type) m
    -> w t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteType bisub wt =
    deferredBisubstitution bisub $ \dbisub -> do
        MkShimWit wt' fconv <- deferBisubstituteType dbisub wt
        return $ MkShimWit wt' $ applyPolarPolyFuncShim fconv (id, id)

singleBisubstitute ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv t.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => TypeVarT tv
    -> PShimWit (pshim Type) (DolanType ground) polarity tv
    -> PShimWit (pshim Type) (DolanType ground) polarity t
    -> PShimWit (pshim Type) (DolanType ground) polarity t
singleBisubstitute var ta (MkShimWit tb conv) = let
    bisub :: Bisubstitution ground (pshim Type) Identity
    bisub = mkSingleBisubstitution True var $ return ta
    in mapPolarShimWit conv $ runIdentity $ bisubstituteType bisub tb

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , BisubstitutablePolyShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanSingularType ground polarity) where
    deferBisubstituteType (MkDeferredBisubstitution _ n mpos mneg) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' =
            case polarityType @polarity of
                PositiveType -> do
                    MkShimWit t conv <- mpos
                    return $ MkShimWit t $ mkPolarPolyFuncShim $ \(convpos, _) -> conv . convpos
                NegativeType -> do
                    MkShimWit t conv <- mneg
                    return $ MkShimWit t $ mkPolarPolyFuncShim $ \(_, convneg) -> conv . convneg
    deferBisubstituteType (MkDeferredBisubstitution _ nb _ _) t@(RecursiveDolanSingularType nt _)
        | Just Refl <- testEquality nb nt = return $ shimWitToDolan $ mkPolarShimWit t
    deferBisubstituteType sub@(MkDeferredBisubstitution isRecursive _ _ _) t@(RecursiveDolanSingularType oldvar pt) = do
        newvar <-
            if isRecursive
                then return $ typeVarName oldvar
                            -- find a name that isn't free in either sub or t,
                else runVarRenamerT (renameableVars t <> renameableVars sub) [] $ do
                         runVarNamespaceT [] FreeName $ varNamespaceTRename $ typeVarName oldvar
        pts <- deferBisubstituteType sub pt
        return $ shimWitToDolan $ recursiveRenameDolanShimWit oldvar newvar pts
    deferBisubstituteType sub t = do
        t' <- mapDolanSingularTypeM (deferBisubstituteType sub) t
        return $ shimWitToDolan t'

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , BisubstitutablePolyShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanType ground polarity) where
    deferBisubstituteType _ NilDolanType = return nilDolanShimWit
    deferBisubstituteType sub (ConsDolanType ta tb) = do
        tfa <- deferBisubstituteType sub ta
        tfb <- deferBisubstituteType sub tb
        return $ joinMeetShimWit tfa tfb

bisubstitutesType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m polarity t.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, MonadInner m, Is PolarityType polarity)
    => [Bisubstitution ground (pshim Type) m]
    -> DolanType ground polarity t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstitutesType [] t = return $ mkPolarShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainPolarShimWitM (bisubstitutesType subs) tf

bisubstitute ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m a.
       ( IsDolanGroundType ground
       , BisubstitutablePolyShim pshim
       , MonadInner m
       , PShimWitMappable (pshim Type) (DolanType ground) a
       )
    => Bisubstitution ground (pshim Type) m
    -> EndoM m a
bisubstitute sub = mapPShimWitsM @_ @(pshim Type) (bisubstituteType sub) (bisubstituteType sub)

bisubstitutes ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m a.
       ( IsDolanGroundType ground
       , BisubstitutablePolyShim pshim
       , MonadInner m
       , PShimWitMappable (pshim Type) (DolanType ground) a
       )
    => [Bisubstitution ground (pshim Type) m]
    -> EndoM m a
bisubstitutes subs = mconcat $ fmap bisubstitute subs

reducePolarMap ::
       forall (pshim :: PolyShimKind) polarity a b c d. (ReduciblePolyShim pshim, Is PolarityType polarity)
    => (PolarMap (ReducedPolyShim pshim Type) polarity a b -> PolarMap (ReducedPolyShim pshim Type) polarity c d)
    -> PolarMap (pshim Type) polarity a b
    -> PolarMap (pshim Type) polarity c d
reducePolarMap f (MkPolarMap conv) =
    MkPolarMap $
    case polarityType @polarity of
        PositiveType -> reduceShim (unPolarMap . f . MkPolarMap) conv
        NegativeType -> reduceShim (unPolarMap . f . MkPolarMap) conv

recursiveBisubstitute ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv a.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => TypeVarT tv
    -> String
    -> DolanType ground polarity a
    -> FuncShimWit (pshim Type) (DolanSingularType ground) polarity tv a
recursiveBisubstitute oldvar recvarname t =
    invertPolarity @polarity $
    withDict (reducedBisubstitutablePolyShim @pshim) $
    newTypeVar recvarname $ \(newvar :: TypeVarT newtv) ->
        case polarityType @polarity of
            PositiveType -> let
                dbisub :: DeferredBisubstitution Identity ground (ReducedPolyShim pshim Type) tv newtv tv
                dbisub =
                    MkDeferredBisubstitution
                        False
                        oldvar
                        (pure $ varDolanShimWit newvar)
                        (pure $ varDolanShimWit oldvar)
                in case runIdentity $ deferBisubstituteType dbisub t of
                       MkShimWit t' rconv ->
                           assignTypeVarWit newvar t' $
                           MkFuncShimWit (RecursiveDolanSingularType newvar t') $
                           reducePolarMap $ \sconv -> let
                               conv = lazyPolarMap $ (applyPolarPolyFuncShim rconv (conv, id)) . sconv
                               in conv
            NegativeType -> let
                dbisub :: DeferredBisubstitution Identity ground (ReducedPolyShim pshim Type) tv tv newtv
                dbisub =
                    MkDeferredBisubstitution
                        False
                        oldvar
                        (pure $ varDolanShimWit oldvar)
                        (pure $ varDolanShimWit newvar)
                in case runIdentity $ deferBisubstituteType dbisub t of
                       MkShimWit t' rconv ->
                           assignTypeVarWit newvar t' $
                           MkFuncShimWit (RecursiveDolanSingularType newvar t') $
                           reducePolarMap $ \sconv -> let
                               conv = lazyPolarMap $ (applyPolarPolyFuncShim rconv (id, conv)) . sconv
                               in conv

recursiveRenameDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => TypeVarT tv
    -> String
    -> PShimWit (pshim Type) (DolanType ground) polarity tv
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity tv
recursiveRenameDolanShimWit oldvar recvarname (MkShimWit t sconv) =
    mapFuncShimWit (recursiveBisubstitute oldvar recvarname t) sconv

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity tv.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => TypeVarT tv
    -> PShimWit (pshim Type) (DolanType ground) polarity tv
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity tv
recursiveDolanShimWit oldvar = recursiveRenameDolanShimWit oldvar (typeVarName oldvar)

mapDolanSingularType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> PShimWit (pshim Type) (DolanType ground) polarity' t')
    -> DolanSingularType ground polarity t
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity t
mapDolanSingularType ff t = runIdentity $ mapDolanSingularTypeM (\t' -> Identity $ ff t') t

mapDolanGroundedTypeM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (Monad m, IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> DolanGroundedType ground polarity t
    -> m (PShimWit (pshim Type) (DolanGroundedType ground) polarity t)
mapDolanGroundedTypeM ff (MkDolanGroundedType g args) = do
    MkShimWit args' conv <- mapDolanArgumentsM ff (groundTypeVarianceMap g) args
    return $ MkShimWit (MkDolanGroundedType g args') conv

mapDolanSingularTypeM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (Monad m, IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
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
