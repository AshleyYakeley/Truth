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
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.VarSubstitute
import Shapes

type Bisubstitution :: GroundTypeKind -> (Type -> Type) -> Type
data Bisubstitution ground m =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (DolanSemiIsoShimWit ground 'Positive (UVar Type name)))
                                  (m (DolanSemiIsoShimWit ground 'Negative (UVar Type name)))

instance forall (ground :: GroundTypeKind) m. (IsDolanGroundType ground, Traversable m) =>
             NamespaceRenamable (DolanTypeSystem ground) (Bisubstitution ground m) where
    namespaceRename (MkBisubstitution var mpos mneg) = do
        mpos' <- for mpos $ dolanNamespaceRename @ground
        mneg' <- for mneg $ dolanNamespaceRename @ground
        return $ MkBisubstitution var mpos' mneg'

mkPolarBisubstitution ::
       forall (ground :: GroundTypeKind) polarity m name. Is PolarityType polarity
    => SymbolType name
    -> m (DolanSemiIsoShimWit ground polarity (UVar Type name))
    -> m (DolanSemiIsoShimWit ground (InvertPolarity polarity) (UVar Type name))
    -> Bisubstitution ground m
mkPolarBisubstitution n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution n a b
        NegativeType -> MkBisubstitution n b a

mkSingleBisubstitution ::
       forall (ground :: GroundTypeKind) polarity m name.
       (IsDolanGroundType ground, Is PolarityType polarity, Applicative m)
    => SymbolType name
    -> m (DolanSemiIsoShimWit ground polarity (UVar Type name))
    -> Bisubstitution ground m
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

class Bisubstitutable (ground :: GroundTypeKind) (polarity :: Polarity) (w :: Type -> Type) | w -> ground polarity where
    bisubstituteType ::
           forall m t. MonadOne m
        => Bisubstitution ground m
        -> w t
        -> m (DolanSemiIsoShimWit ground polarity t)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) (shim :: ShimKind Type). ( Bisubstitutable ground polarity (w polarity)
         , Is PolarityType polarity
         , shim ~ DolanPolySemiIsoShim ground Type
         , InCategory shim
         ) => Bisubstitutable ground polarity (PShimWit shim w polarity) where
    bisubstituteType sub = chainShimWitM $ bisubstituteType sub

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanSingularType ground polarity) where
    bisubstituteType (MkBisubstitution n tp tq) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' = getBisubstitution tp tq
    bisubstituteType sub t = do
        t' <- mapDolanSingularTypeM (bisubstituteType sub) t
        return $ singleDolanShimWit t'

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanPlainType ground polarity) where
    bisubstituteType _ NilDolanPlainType = return $ mkShimWit $ PlainDolanType NilDolanPlainType
    bisubstituteType sub (ConsDolanPlainType ta tb) = do
        tfa <- bisubstituteType sub ta
        tfb <- bisubstituteType sub tb
        return $ joinMeetSemiIsoShimWit tfa tfb

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanType ground polarity) where
    bisubstituteType ::
           forall m t. MonadOne m
        => Bisubstitution ground m
        -> DolanType ground polarity t
        -> m (DolanSemiIsoShimWit ground polarity t)
    bisubstituteType sub (PlainDolanType pt) = bisubstituteType sub pt
    bisubstituteType (MkBisubstitution nb _ _) t@(RecursiveDolanType nt _)
        | Just Refl <- testEquality nb nt = return $ mkShimWit t
    bisubstituteType sub t@(RecursiveDolanType oldvar pt) =
        invertPolarity @polarity $ let
            -- find a name that isn't free in either sub or t,
            -- if possible the same name as oldvar
            newname =
                runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- dolanNamespaceRename @ground t
                        _ <- dolanNamespaceRename @ground sub
                        return ()
                    varRenamerTGenerate [uVarName oldvar]
            in newUVar newname $ \newvar ->
                   case varSubstitute (mkPolarVarSubstitution @polarity oldvar newvar) pt of
                       MkShimWit pt' vconv -> do
                           MkShimWit t' sconv <- bisubstituteType sub pt'
                           assignUVarWit newvar t' $ do
                               return $
                                   MkShimWit (recursiveDolanType newvar t') $ let
                                       conv = sconv <.> (applyPolarPolyFuncShim vconv (lazyPolarSemiIso conv, id))
                                       in conv

{-
recursiveDolanType
            pt' <- dolanNamespaceRename @ground pt
            t' <- lift $ lift $ bisubstituteType sub pt'
            return $ mkShimWit (RecursiveDolanType newvar t')


    varSubstitute sub t@(RecursiveDolanType oldvar pt) = invertPolarity @polarity $ let
            -- find a name that isn't free in either sub or t,
            -- if possible the same name as oldvar
            newname = runIdentity $
                runVarRenamerT $ do
                    runVarNamespaceT $ do
                        _ <- dolanNamespaceRename @ground t
                        _ <- dolanNamespaceRename @ground sub
                        return ()
                    varRenamerTGenerate [uVarName oldvar]
            in newUVar newname $ \newvar -> case varSubstitute (mkPolarVarSubstitution @polarity oldvar newvar) pt of
                MkShimWit pt' vconv -> case varSubstitute sub pt' of
                    MkShimWit pt'' sconv -> assignUVarWit newvar pt'' $ MkShimWit (RecursiveDolanType newvar pt'') $
                        mkPolarPolyFuncShim $ \vars -> let
                            conv = applyPolarPolyFuncShim sconv vars <.> applyPolarPolyFuncShim vconv (lazyPolarSemiIso conv,cid)
                            in conv
-}
bisubstitutesType ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, MonadOne m, Is PolarityType polarity)
    => [Bisubstitution ground m]
    -> DolanType ground polarity t
    -> m (DolanSemiIsoShimWit ground polarity t)
bisubstitutesType [] t = return $ mkShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainShimWitM (bisubstitutesType subs) tf

bisubstitutes ::
       forall (ground :: GroundTypeKind) m a.
       (IsDolanGroundType ground, MonadOne m, PShimWitMappable (DolanPolyShim ground Type) (DolanType ground) a)
    => [Bisubstitution ground m]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <-
        mapPShimWitsM
            (fmap (reshimWit polySemiIsoForwards) . bisubstituteType sub)
            (fmap (reshimWit polySemiIsoForwards) . bisubstituteType sub)
            expr
    bisubstitutes subs expr'
