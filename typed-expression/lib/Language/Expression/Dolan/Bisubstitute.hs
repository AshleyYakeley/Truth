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
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.MapType
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type DeferredShim :: PolyShimKind -> Type -> Type -> Type -> ShimKind Type
type DeferredShim pshim oldtype newtypepos newtypeneg
     = PolyFuncShim ( PolarMap (pshim Type) 'Positive oldtype newtypepos
                    , PolarMap (pshim Type) 'Negative oldtype newtypeneg) pshim Type

type DeferredBisubstitution :: (Type -> Type) -> GroundTypeKind -> PolyShimKind -> Symbol -> Type -> Type -> Type
data DeferredBisubstitution m ground pshim oldname newtypepos newtypeneg where
    MkDeferredBisubstitution
        :: forall m (ground :: GroundTypeKind) (pshim :: PolyShimKind) oldname newtypepos newtypeneg.
           SymbolType oldname
        -> m (PShimWit (pshim Type) (DolanType ground) 'Positive newtypepos)
        -> m (PShimWit (pshim Type) (DolanType ground) 'Negative newtypeneg)
        -> DeferredBisubstitution m ground pshim oldname newtypepos newtypeneg

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m oldname newtypepos newtypeneg. ( IsDolanGroundType ground
         , Traversable m
         ) =>
             NamespaceRenamable (DolanTypeSystem ground) (DeferredBisubstitution m ground pshim oldname newtypepos newtypeneg) where
    namespaceRename (MkDeferredBisubstitution var mpos mneg) = do
        mpos' <- for mpos $ dolanNamespaceRename @ground
        mneg' <- for mneg $ dolanNamespaceRename @ground
        return $ MkDeferredBisubstitution var mpos' mneg'

type Bisubstitution :: GroundTypeKind -> PolyShimKind -> (Type -> Type) -> Type
data Bisubstitution ground pshim m =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (PShimWit (pshim Type) (DolanType ground) 'Positive (UVar Type name)))
                                  (m (PShimWit (pshim Type) (DolanType ground) 'Negative (UVar Type name)))

deferredBisubstitution ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m r.
       Bisubstitution ground pshim m
    -> (forall name. DeferredBisubstitution m ground pshim name (UVar Type name) (UVar Type name) -> r)
    -> r
deferredBisubstitution (MkBisubstitution var mpos mneg) call = call $ MkDeferredBisubstitution var mpos mneg

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m. (IsDolanGroundType ground, Traversable m) =>
             NamespaceRenamable (DolanTypeSystem ground) (Bisubstitution ground pshim m) where
    namespaceRename (MkBisubstitution var mpos mneg) = do
        mpos' <- for mpos $ dolanNamespaceRename @ground
        mneg' <- for mneg $ dolanNamespaceRename @ground
        return $ MkBisubstitution var mpos' mneg'

mkPolarBisubstitution ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity m name. Is PolarityType polarity
    => SymbolType name
    -> m (PShimWit (pshim Type) (DolanType ground) polarity (UVar Type name))
    -> m (PShimWit (pshim Type) (DolanType ground) (InvertPolarity polarity) (UVar Type name))
    -> Bisubstitution ground pshim m
mkPolarBisubstitution n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution n a b
        NegativeType -> MkBisubstitution n b a

mkSingleBisubstitution ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity m name.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity, Applicative m)
    => SymbolType name
    -> m (PShimWit (pshim Type) (DolanType ground) polarity (UVar Type name))
    -> Bisubstitution ground pshim m
mkSingleBisubstitution var mw =
    invertPolarity @polarity $
    mkPolarBisubstitution var mw $ pure $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType var

class Bisubstitutable (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type)
    | w -> ground polarity
    where
    deferBisubstituteType ::
           forall m oldname newtypepos newtypeneg t. (MonadOne m)
        => DeferredBisubstitution m ground pshim oldname newtypepos newtypeneg
        -> w t
        -> m (PShimWit (DeferredShim pshim (UVar Type oldname) newtypepos newtypeneg) (DolanType ground) polarity t)

bisubstituteType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type) m t.
       (GenShim pshim, Bisubstitutable ground pshim polarity w, Is PolarityType polarity, MonadOne m)
    => Bisubstitution ground pshim m
    -> w t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteType bisub wt =
    deferredBisubstitution bisub $ \dbisub -> do
        MkShimWit wt' fconv <- deferBisubstituteType dbisub wt
        return $ MkShimWit wt' $ applyPolarPolyFuncShim fconv (id, id)

bisubstituteShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) m t.
       (Bisubstitutable ground pshim polarity (w polarity), GenShim pshim, Is PolarityType polarity, MonadOne m)
    => Bisubstitution ground pshim m
    -> PShimWit (pshim Type) w polarity t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstituteShimWit sub = chainShimWitM $ bisubstituteType sub

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , GenShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanSingularType ground polarity) where
    deferBisubstituteType (MkDeferredBisubstitution n mpos mneg) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' =
            case polarityType @polarity of
                PositiveType -> do
                    MkShimWit t conv <- mpos
                    return $ MkShimWit t $ mkPolarPolyFuncShim $ \(convpos, _) -> conv . convpos
                NegativeType -> do
                    MkShimWit t conv <- mneg
                    return $ MkShimWit t $ mkPolarPolyFuncShim $ \(_, convneg) -> conv . convneg
    deferBisubstituteType (MkDeferredBisubstitution nb _ _) t@(RecursiveDolanSingularType nt _)
        | Just Refl <- testEquality nb nt = return $ singleDolanShimWit $ mkShimWit t
    deferBisubstituteType sub t@(RecursiveDolanSingularType oldvar pt) =
        runVarRenamerT $ do
            runVarNamespaceT $ do
                -- find a name that isn't free in either sub or t,
                -- if possible the same name as oldvar
                _ <- dolanNamespaceRename @ground t
                _ <- dolanNamespaceRename @ground sub
                MkVarType newvar :: VarType ntype <- varNamespaceTRenameUVar @Type oldvar
                pt' <- dolanNamespaceRename @ground pt
                MkShimWit pt'' conv <- lift $ lift $ deferBisubstituteType sub pt'
                assignUVar @Type @ntype oldvar $
                    return $
                    mapShimWit (shimMapRecursive conv) $
                    singleDolanShimWit $ mkShimWit $ RecursiveDolanSingularType newvar pt''
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
    => [Bisubstitution ground pshim m]
    -> DolanType ground polarity t
    -> m (PShimWit (pshim Type) (DolanType ground) polarity t)
bisubstitutesType [] t = return $ mkShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainShimWitM (bisubstitutesType subs) tf

bisubstitutes ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m a.
       (IsDolanGroundType ground, GenShim pshim, MonadOne m, PShimWitMappable (pshim Type) (DolanType ground) a)
    => [Bisubstitution ground pshim m]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- mapPShimWitsM @_ @(pshim Type) (bisubstituteType sub) (bisubstituteType sub) expr
    bisubstitutes subs expr'
