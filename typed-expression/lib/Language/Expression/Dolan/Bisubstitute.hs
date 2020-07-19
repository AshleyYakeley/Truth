module Language.Expression.Dolan.Bisubstitute
    ( Bisubstitution(..)
    , mkPolarBisubstitution
    , mkSingleBisubstitution
    , Bisubstitutable(..)
    , bisubstitutesType
    , bisubstitutes
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.VarSubstitute
import Shapes

type Bisubstitution :: GroundTypeKind -> PolyShimKind -> (Type -> Type) -> Type
data Bisubstitution ground pshim m =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (PShimWit (pshim Type) (DolanType ground) 'Positive (UVar Type name)))
                                  (m (PShimWit (pshim Type) (DolanType ground) 'Negative (UVar Type name)))

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

getBisubstitution ::
       forall (polarity :: Polarity) (shim :: ShimKind Type) m (wit :: Polarity -> Type -> Type) t.
       Is PolarityType polarity
    => m (PShimWit shim wit 'Positive t)
    -> m (PShimWit shim wit 'Negative t)
    -> m (PShimWit shim wit polarity t)
getBisubstitution tp tq =
    case polarityType @polarity of
        PositiveType -> tp
        NegativeType -> tq

class Bisubstitutable (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Type -> Type)
    | w -> ground polarity
    where
    bisubstituteType ::
           forall m t. (MonadOne m)
        => Bisubstitution ground pshim m
        -> w t
        -> m (PShimWit (pshim Type) (DolanType ground) polarity t)

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type). ( Bisubstitutable ground pshim polarity (w polarity)
         , GenShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (PShimWit (pshim Type) w polarity) where
    bisubstituteType sub = chainShimWitM $ bisubstituteType sub

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , GenShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanSingularType ground polarity) where
    bisubstituteType (MkBisubstitution n tp tq) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' = getBisubstitution tp tq
    bisubstituteType (MkBisubstitution nb _ _) t@(RecursiveDolanSingularType nt _)
        | Just Refl <- testEquality nb nt = return $ singleDolanShimWit $ mkShimWit t
    bisubstituteType sub t@(RecursiveDolanSingularType oldvar pt) =
        runVarRenamerT $ do
            runVarNamespaceT $ do
                -- find a name that isn't free in either sub or t,
                -- if possible the same name as oldvar
                _ <- dolanNamespaceRename @ground t
                _ <- dolanNamespaceRename @ground sub
                MkVarType newvar :: VarType ntype <- varNamespaceTRenameUVar @Type oldvar
                pt' <- dolanNamespaceRename @ground pt
                MkShimWit pt'' conv <- lift $ lift $ bisubstituteType sub pt'
                assignUVar @Type @ntype oldvar $
                    return $
                    mapShimWit (shimMapRecursive conv) $
                    singleDolanShimWit $ mkShimWit $ RecursiveDolanSingularType newvar pt''
    bisubstituteType sub t = do
        t' <- mapDolanSingularTypeM (bisubstituteType sub) t
        return $ singleDolanShimWit t'

instance forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity. ( IsDolanGroundType ground
         , GenShim pshim
         , Is PolarityType polarity
         ) => Bisubstitutable ground pshim polarity (DolanType ground polarity) where
    bisubstituteType _ NilDolanType = return $ mkShimWit NilDolanType
    bisubstituteType sub (ConsDolanType ta tb) = do
        tfa <- bisubstituteType sub ta
        tfb <- bisubstituteType sub tb
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
