module Language.Expression.Dolan.Bisubstitute
    ( DeferredBisubstitution(..)
    , Bisubstitution(..)
    , mkPolarBisubstitution
    , mkSingleBisubstitution
    , Bisubstitutable(..)
    , bisubstituteType
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
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type DeferredShim :: PolyShimKind -> Type -> Type -> Type -> ShimKind Type
type DeferredShim pshim oldtype newtypepos newtypeneg
     = PolyFuncShim ( PolarMap (pshim Type) 'Positive oldtype newtypepos
                    , PolarMap (pshim Type) 'Negative oldtype newtypeneg) pshim Type

type DeferredBisubstitution :: (Type -> Type) -> GroundTypeKind -> ShimKind Type -> Symbol -> Type -> Type -> Type
data DeferredBisubstitution m ground shim oldname newtypepos newtypeneg where
    MkDeferredBisubstitution
        :: forall m (ground :: GroundTypeKind) (shim :: ShimKind Type) oldname newtypepos newtypeneg.
           Bool
        -> SymbolType oldname
        -> m (PShimWit shim (DolanType ground) 'Positive newtypepos)
        -> m (PShimWit shim (DolanType ground) 'Negative newtypeneg)
        -> DeferredBisubstitution m ground shim oldname newtypepos newtypeneg

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m oldname newtypepos newtypeneg. ( IsDolanGroundType ground
         , Traversable m
         ) =>
             NamespaceRenamable (DolanTypeSystem ground) (DeferredBisubstitution m ground shim oldname newtypepos newtypeneg) where
    namespaceRename (MkDeferredBisubstitution isRecursive var mpos mneg) = do
        mpos' <- for mpos $ dolanNamespaceRename @ground
        mneg' <- for mneg $ dolanNamespaceRename @ground
        return $ MkDeferredBisubstitution isRecursive var mpos' mneg'
    namespaceTypeNames (MkDeferredBisubstitution _ _ mpos mneg) =
        mconcat (fmap (dolanNamespaceTypeNames @ground) (toList mpos)) <>
        mconcat (fmap (dolanNamespaceTypeNames @ground) (toList mneg))

type Bisubstitution :: GroundTypeKind -> ShimKind Type -> (Type -> Type) -> Type
data Bisubstitution ground shim m =
    forall name. MkBisubstitution Bool
                                  (SymbolType name)
                                  (m (PShimWit shim (DolanType ground) 'Positive (UVarT name)))
                                  (m (PShimWit shim (DolanType ground) 'Negative (UVarT name)))

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
                Nothing -> "fails"
        sneg =
            case mToMaybe mtneg of
                Just (MkShimWit t _) -> allShow t
                Nothing -> "fails"
        in show var <> srec <> " => " <> "(" <> spos <> "," <> sneg <> ")"

deferredBisubstitution ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m r.
       Bisubstitution ground shim m
    -> (forall name. DeferredBisubstitution m ground shim name (UVarT name) (UVarT name) -> r)
    -> r
deferredBisubstitution (MkBisubstitution isRecursive var mpos mneg) call =
    call $ MkDeferredBisubstitution isRecursive var mpos mneg

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m. (IsDolanGroundType ground, Traversable m) =>
             NamespaceRenamable (DolanTypeSystem ground) (Bisubstitution ground shim m) where
    namespaceRename (MkBisubstitution isRecursive var mpos mneg) = do
        mpos' <- for mpos $ dolanNamespaceRename @ground
        mneg' <- for mneg $ dolanNamespaceRename @ground
        return $ MkBisubstitution isRecursive var mpos' mneg'
    namespaceTypeNames (MkBisubstitution _ _ mpos mneg) =
        mconcat (fmap (dolanNamespaceTypeNames @ground) (toList mpos)) <>
        mconcat (fmap (dolanNamespaceTypeNames @ground) (toList mneg))

mkPolarBisubstitution ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) polarity m name. Is PolarityType polarity
    => Bool
    -> SymbolType name
    -> m (PShimWit shim (DolanType ground) polarity (UVarT name))
    -> m (PShimWit shim (DolanType ground) (InvertPolarity polarity) (UVarT name))
    -> Bisubstitution ground shim m
mkPolarBisubstitution isRecursive n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution isRecursive n a b
        NegativeType -> MkBisubstitution isRecursive n b a

mkSingleBisubstitution ::
       forall (ground :: GroundTypeKind) (shim :: ShimKind Type) polarity m name.
       (IsDolanGroundType ground, JoinMeetIsoCategory shim, Is PolarityType polarity, Applicative m)
    => Bool
    -> SymbolType name
    -> m (PShimWit shim (DolanType ground) polarity (UVarT name))
    -> Bisubstitution ground shim m
mkSingleBisubstitution isRecursive var mw =
    invertPolarity @polarity $ mkPolarBisubstitution isRecursive var mw $ pure $ varDolanShimWit var

class Bisubstitutable (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type)
    | w -> ground polarity
    where
    deferBisubstituteType ::
           forall m oldname newtypepos newtypeneg t. MonadInner m
        => DeferredBisubstitution m ground (pshim Type) oldname newtypepos newtypeneg
        -> w t
        -> m (PShimWit (DeferredShim pshim (UVarT oldname) newtypepos newtypeneg) (DolanType ground) polarity t)

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
                then return $ uVarName oldvar
                            -- find a name that isn't free in either sub or t,
                else runVarRenamerT (dolanNamespaceTypeNames @ground t <> dolanNamespaceTypeNames @ground sub) $ do
                         runVarNamespaceT FreeName $ varNamespaceTRename $ uVarName oldvar
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
    -> a
    -> m a
bisubstitute sub expr = mapPShimWitsM @_ @(pshim Type) (bisubstituteType sub) (bisubstituteType sub) expr

bisubstitutes ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m a.
       ( IsDolanGroundType ground
       , BisubstitutablePolyShim pshim
       , MonadInner m
       , PShimWitMappable (pshim Type) (DolanType ground) a
       )
    => [Bisubstitution ground (pshim Type) m]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- bisubstitute sub expr
    bisubstitutes subs expr'

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
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name a.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => SymbolType name
    -> String
    -> DolanType ground polarity a
    -> FuncShimWit (pshim Type) (DolanSingularType ground) polarity (UVarT name) a
recursiveBisubstitute oldvar recvarname t =
    invertPolarity @polarity $
    withDict (reducedBisubstitutablePolyShim @pshim) $
    newUVar recvarname $ \(newvar :: SymbolType newname) ->
        case polarityType @polarity of
            PositiveType -> let
                dbisub ::
                       DeferredBisubstitution Identity ground (ReducedPolyShim pshim Type) name (UVarT newname) (UVarT name)
                dbisub =
                    MkDeferredBisubstitution
                        False
                        oldvar
                        (pure $ varDolanShimWit newvar)
                        (pure $ varDolanShimWit oldvar)
                in case runIdentity $ deferBisubstituteType dbisub t of
                       MkShimWit t' rconv ->
                           assignUVarWit newvar t' $
                           MkFuncShimWit (RecursiveDolanSingularType newvar t') $
                           reducePolarMap $ \sconv -> let
                               conv = lazyPolarMap $ (applyPolarPolyFuncShim rconv (conv, id)) . sconv
                               in conv
            NegativeType -> let
                dbisub ::
                       DeferredBisubstitution Identity ground (ReducedPolyShim pshim Type) name (UVarT name) (UVarT newname)
                dbisub =
                    MkDeferredBisubstitution
                        False
                        oldvar
                        (pure $ varDolanShimWit oldvar)
                        (pure $ varDolanShimWit newvar)
                in case runIdentity $ deferBisubstituteType dbisub t of
                       MkShimWit t' rconv ->
                           assignUVarWit newvar t' $
                           MkFuncShimWit (RecursiveDolanSingularType newvar t') $
                           reducePolarMap $ \sconv -> let
                               conv = lazyPolarMap $ (applyPolarPolyFuncShim rconv (id, conv)) . sconv
                               in conv

recursiveRenameDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => SymbolType name
    -> String
    -> PShimWit (pshim Type) (DolanType ground) polarity (UVarT name)
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity (UVarT name)
recursiveRenameDolanShimWit oldvar recvarname (MkShimWit t sconv) =
    mapFuncShimWit (recursiveBisubstitute oldvar recvarname t) sconv

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name.
       (IsDolanGroundType ground, BisubstitutablePolyShim pshim, Is PolarityType polarity)
    => SymbolType name
    -> PShimWit (pshim Type) (DolanType ground) polarity (UVarT name)
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity (UVarT name)
recursiveDolanShimWit oldvar = recursiveRenameDolanShimWit oldvar (uVarName oldvar)

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
