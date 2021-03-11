module Language.Expression.Dolan.Bisubstitute
    ( DeferredBisubstitution(..)
    , Bisubstitution(..)
    , mkPolarBisubstitution
    , mkSingleBisubstitution
    , Bisubstitutable(..)
    , bisubstituteType
    , bisubstituteShimWit
    , bisubstitutesType
    , bisubstitutes
    , GenShim
    , recursiveDolanShimWit
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
import Language.Expression.Dolan.Variance
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

type Bisubstitution :: GroundTypeKind -> ShimKind Type -> (Type -> Type) -> Type
data Bisubstitution ground shim m =
    forall name. MkBisubstitution Bool
                                  (SymbolType name)
                                  (m (PShimWit shim (DolanType ground) 'Positive (UVarT name)))
                                  (m (PShimWit shim (DolanType ground) 'Negative (UVarT name)))

instance forall (ground :: GroundTypeKind) (shim :: ShimKind Type) m. ( MonadOne m
         , AllWitnessConstraint Show (DolanType ground 'Positive)
         , AllWitnessConstraint Show (DolanType ground 'Negative)
         ) => Show (Bisubstitution ground shim m) where
    show (MkBisubstitution isRecursive var mtpos mtneg) = let
        srec =
            if isRecursive
                then " (recursive)"
                else ""
        spos =
            case getMaybeOne mtpos of
                Just (MkShimWit t _) -> showAllWitness t
                Nothing -> "fails"
        sneg =
            case getMaybeOne mtneg of
                Just (MkShimWit t _) -> showAllWitness t
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
           forall m oldname newtypepos newtypeneg t. (MonadOne m)
        => DeferredBisubstitution m ground (pshim Type) oldname newtypepos newtypeneg
        -> w t
        -> m (PShimWit (DeferredShim pshim (UVarT oldname) newtypepos newtypeneg) (DolanType ground) polarity t)

bisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) m t.
       (GenShim pshim, Bisubstitutable ground pshim polarity w, Is PolarityType polarity, MonadOne m)
    => Bisubstitution ground (pshim Type) m
    -> w t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteType bisub wt =
    deferredBisubstitution bisub $ \dbisub -> do
        MkShimWit wt' fconv <- deferBisubstituteType dbisub wt
        return $ MkShimWit wt' $ applyPolarPolyFuncShim fconv (id, id)

bisubstituteShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) m t.
       (Bisubstitutable ground pshim polarity (w polarity), GenShim pshim, Is PolarityType polarity, MonadOne m)
    => Bisubstitution ground (pshim Type) m
    -> PShimWit (pshim Type) w polarity t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteShimWit sub = chainShimWitM $ bisubstituteType sub

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , GenShim pshim
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
        | Just Refl <- testEquality nb nt = return $ singleDolanShimWit $ mkShimWit t
    deferBisubstituteType sub@(MkDeferredBisubstitution isRecursive _ _ _) t@(RecursiveDolanSingularType oldvar pt) = do
        (MkVarType newvar, pt') <-
            if isRecursive
                then return (MkVarType oldvar, pt)
                else runVarRenamerT $ do
                         runVarNamespaceT False $ do
                -- find a name that isn't free in either sub or t,
                -- if possible the same name as oldvar
                             _ <- dolanNamespaceRename @ground t
                             _ <- dolanNamespaceRename @ground sub
                             newvart <- varNamespaceTRenameUVar @Type oldvar
                             pt' <- dolanNamespaceRename @ground pt
                             return (newvart, pt')
        pts <- deferBisubstituteType sub pt'
        return $ singleDolanShimWit $ recursiveDolanShimWit newvar pts
    deferBisubstituteType sub t = do
        t' <- mapDolanSingularTypeM (deferBisubstituteType sub) t
        return $ singleDolanShimWit t'

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , GenShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanType ground polarity) where
    deferBisubstituteType _ NilDolanType = return $ mkShimWit NilDolanType
    deferBisubstituteType sub (ConsDolanType ta tb) = do
        tfa <- deferBisubstituteType sub ta
        tfb <- deferBisubstituteType sub tb
        return $ joinMeetShimWit tfa tfb

bisubstitutesType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m polarity t.
       (IsDolanGroundType ground, GenShim pshim, MonadOne m, Is PolarityType polarity)
    => [Bisubstitution ground (pshim Type) m]
    -> DolanType ground polarity t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstitutesType [] t = return $ mkShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainShimWitM (bisubstitutesType subs) tf

bisubstitutes ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m a.
       (IsDolanGroundType ground, GenShim pshim, MonadOne m, PShimWitMappable (pshim Type) (DolanType ground) a)
    => [Bisubstitution ground (pshim Type) m]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- mapPShimWitsM @_ @(pshim Type) (bisubstituteType sub) (bisubstituteType sub) expr
    bisubstitutes subs expr'

type GenShim :: PolyShimKind -> Constraint
type GenShim pshim = (LazyCategory (pshim Type), IsoMapShim (pshim Type), DolanVarianceInCategory pshim)

recursiveDolanShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity name.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => SymbolType name
    -> PShimWit (pshim Type) (DolanType ground) polarity (UVarT name)
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity (UVarT name)
recursiveDolanShimWit oldvar (MkShimWit t sconv) =
    invertPolarity @polarity $
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        case polarityType @polarity of
            PositiveType -> let
                dbisub :: DeferredBisubstitution Identity ground (pshim Type) name (UVarT newname) (UVarT name)
                dbisub =
                    MkDeferredBisubstitution
                        False
                        oldvar
                        (pure $ varDolanShimWit newvar)
                        (pure $ varDolanShimWit oldvar)
                in case runIdentity $ deferBisubstituteType dbisub t of
                       MkShimWit t' rconv ->
                           assignUVarWit newvar t' $ let
                               conv = lazyPolarMap $ (applyPolarPolyFuncShim rconv (conv, id)) . sconv
                               in MkShimWit (RecursiveDolanSingularType newvar t') conv
            NegativeType -> let
                dbisub :: DeferredBisubstitution Identity ground (pshim Type) name (UVarT name) (UVarT newname)
                dbisub =
                    MkDeferredBisubstitution
                        False
                        oldvar
                        (pure $ varDolanShimWit oldvar)
                        (pure $ varDolanShimWit newvar)
                in case runIdentity $ deferBisubstituteType dbisub t of
                       MkShimWit t' rconv ->
                           assignUVarWit newvar t' $ let
                               conv = lazyPolarMap $ (applyPolarPolyFuncShim rconv (id, conv)) . sconv
                               in MkShimWit (RecursiveDolanSingularType newvar t') conv

mapDolanGroundArguments ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity dv gt t.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> PShimWit (pshim Type) (DolanType ground) polarity' t')
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity t
mapDolanGroundArguments ff g args =
    case mapDolanArguments ff (groundTypeVarianceType g) (groundTypeVarianceMap g) args of
        MkShimWit args' conv -> MkShimWit (GroundDolanSingularType g args') conv

mapDolanSingularType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> PShimWit (pshim Type) (DolanType ground) polarity' t')
    -> DolanSingularType ground polarity t
    -> PShimWit (pshim Type) (DolanSingularType ground) polarity t
mapDolanSingularType ff (GroundDolanSingularType gt args) = mapDolanGroundArguments ff gt args
mapDolanSingularType _ t@(VarDolanSingularType _) = mkShimWit t
mapDolanSingularType ff (RecursiveDolanSingularType var t) = recursiveDolanShimWit var $ ff t

mapDolanGroundArgumentsM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity dv gt t.
       (Monad m, IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> m (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanGroundArgumentsM ff g args = do
    MkShimWit args' conv <- mapDolanArgumentsM ff (groundTypeVarianceType g) (groundTypeVarianceMap g) args
    return $ MkShimWit (GroundDolanSingularType g args') conv

mapDolanSingularTypeM ::
       forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity t.
       (Monad m, IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity)
    => (forall polarity' t'.
            Is PolarityType polarity' =>
                    DolanType ground polarity' t' -> m (PShimWit (pshim Type) (DolanType ground) polarity' t'))
    -> DolanSingularType ground polarity t
    -> m (PShimWit (pshim Type) (DolanSingularType ground) polarity t)
mapDolanSingularTypeM ff (GroundDolanSingularType gt args) = mapDolanGroundArgumentsM ff gt args
mapDolanSingularTypeM _ t@(VarDolanSingularType _) = return $ mkShimWit t
mapDolanSingularTypeM ff (RecursiveDolanSingularType var t) = do
    t' <- ff t
    return $ recursiveDolanShimWit var t'
