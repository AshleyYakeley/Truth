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
    , deferBisubstituteTypeUSubPositive
    , substituteShim
    , substituteApplyFunctor
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
                                  (m (PShimWit (pshim Type) (DolanType ground) 'Positive (UVarT name)))
                                  (m (PShimWit (pshim Type) (DolanType ground) 'Negative (UVarT name)))

deferredBisubstitution ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m r.
       Bisubstitution ground pshim m
    -> (forall name. DeferredBisubstitution m ground pshim name (UVarT name) (UVarT name) -> r)
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
    -> m (PShimWit (pshim Type) (DolanType ground) polarity (UVarT name))
    -> m (PShimWit (pshim Type) (DolanType ground) (InvertPolarity polarity) (UVarT name))
    -> Bisubstitution ground pshim m
mkPolarBisubstitution n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution n a b
        NegativeType -> MkBisubstitution n b a

mkSingleBisubstitution ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity m name.
       (IsDolanGroundType ground, GenShim pshim, Is PolarityType polarity, Applicative m)
    => SymbolType name
    -> m (PShimWit (pshim Type) (DolanType ground) polarity (UVarT name))
    -> Bisubstitution ground pshim m
mkSingleBisubstitution var mw = invertPolarity @polarity $ mkPolarBisubstitution var mw $ pure $ varDolanShimWit var

class Bisubstitutable (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type)
    | w -> ground polarity
    where
    deferBisubstituteType ::
           forall m oldname newtypepos newtypeneg t. (MonadOne m)
        => DeferredBisubstitution m ground pshim oldname newtypepos newtypeneg
        -> w t
        -> m (PShimWit (DeferredShim pshim (UVarT oldname) newtypepos newtypeneg) (DolanType ground) polarity t)

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
                    withRefl (usubResub oldvar newvar pt'') $
                    return $
                    mapShimWit (shimMapRecursive oldvar conv) $
                    singleDolanShimWit $
                    mkShimWit $ RecursiveDolanSingularType newvar $ reflWitCat (invert $ renameUSub oldvar newvar) pt''
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

deferBisubstituteTypeUSubPositive ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m oldname newtypepos w t.
       (Bisubstitutable ground pshim 'Positive w, MonadOne m)
    => DeferredBisubstitution m ground pshim oldname newtypepos (UVarT oldname)
    -> w t
    -> m ( DolanType ground 'Positive (Apply (USub oldname t) newtypepos)
         , PolarMap (DeferredShim pshim (UVarT oldname) newtypepos (UVarT oldname)) 'Positive t (Apply (USub oldname t) newtypepos))
deferBisubstituteTypeUSubPositive dbisub wt = do
    MkShimWit (wt' :: _ t') fconv <- deferBisubstituteType dbisub wt
    Refl <- return $ assignUSub @oldname @t @newtypepos @t'
    return (wt', fconv)

deferBisubstituteTypeUSubNegative ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) m oldname newtypeneg w t.
       (Bisubstitutable ground pshim 'Negative w, MonadOne m)
    => DeferredBisubstitution m ground pshim oldname (UVarT oldname) newtypeneg
    -> w t
    -> m ( DolanType ground 'Negative (Apply (USub oldname t) newtypeneg)
         , PolarMap (DeferredShim pshim (UVarT oldname) (UVarT oldname) newtypeneg) 'Negative t (Apply (USub oldname t) newtypeneg))
deferBisubstituteTypeUSubNegative dbisub wt = do
    MkShimWit (wt' :: _ t') fconv <- deferBisubstituteType dbisub wt
    Refl <- return $ assignUSub @oldname @t @newtypeneg @t'
    return (wt', fconv)

substituteShim ::
       forall (ground :: GroundTypeKind) polarity (w :: Type -> Type) oldname newt t.
       (IsDolanGroundType ground, Is PolarityType polarity, Bisubstitutable ground (DolanPolyShim ground) polarity w)
    => SymbolType oldname
    -> DolanShimWit ground polarity newt
    -> w t
    -> ( DolanType ground polarity (Apply (USub oldname t) newt)
       , (PolarMap (DolanPolyShim ground Type) polarity (UVarT oldname) newt) -> PolarMap (DolanPolyShim ground Type) polarity t (Apply (USub oldname t) newt))
substituteShim oldvar newwit wt =
    case polarityType @polarity of
        PositiveType -> let
            dbisub :: DeferredBisubstitution Identity ground (DolanPolyShim ground) oldname newt (UVarT oldname)
            dbisub = MkDeferredBisubstitution oldvar (pure newwit) (pure $ varDolanShimWit oldvar)
            (t', fconv) = runIdentity $ deferBisubstituteTypeUSubPositive @ground @(DolanPolyShim ground) dbisub wt
            afmap ab = applyPolarPolyFuncShim fconv (ab, id)
            in (t', afmap)
        NegativeType -> let
            dbisub :: DeferredBisubstitution Identity ground (DolanPolyShim ground) oldname (UVarT oldname) newt
            dbisub = MkDeferredBisubstitution oldvar (pure $ varDolanShimWit oldvar) (pure newwit)
            (t', fconv) = runIdentity $ deferBisubstituteTypeUSubNegative @ground @(DolanPolyShim ground) dbisub wt
            afmap ab = applyPolarPolyFuncShim fconv (id, ab)
            in (t', afmap)

substituteApplyFunctor ::
       forall (ground :: GroundTypeKind) polarity (w :: Type -> Type) name t.
       (IsDolanGroundType ground, Is PolarityType polarity, Bisubstitutable ground (DolanPolyShim ground) polarity w)
    => SymbolType name
    -> w t
    -> ApplyFunctor (USub name t)
substituteApplyFunctor oldvar wt =
    newUVar (uVarName oldvar) $ \newvar ->
        case substituteShim @ground @polarity oldvar (varDolanShimWit newvar) wt of
            (_, fconv) ->
                withRefl (usubIdentity @t oldvar) $
                case polarityType @polarity of
                    PositiveType ->
                        MkApplyFunctor $ \(ab :: a -> b) ->
                            assignUVar @Type @a oldvar $
                            assignUVar @Type @b newvar $
                            shimToFunction $ unPolarMap $ fconv $ MkPolarMap $ functionToShim "apply-functor" ab
                    NegativeType ->
                        MkApplyFunctor $ \(ab :: a -> b) ->
                            assignUVar @Type @b oldvar $
                            assignUVar @Type @a newvar $
                            shimToFunction $ unPolarMap $ fconv $ MkPolarMap $ functionToShim "apply-functor" ab
