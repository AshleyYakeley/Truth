module Language.Expression.Dolan.Bisubstitute
    ( Bisubstitution(..)
    , mkPolarBisubstitution
    , bisubstituteSingularType
    , bisubstituteType
    , bisubstituteShimWit
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
import Language.Expression.TypeVariable
import Shapes

type Bisubstitution :: GroundTypeKind -> (Type -> Type) -> Type
data Bisubstitution ground m =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (DolanShimWit ground 'Positive (UVar name)))
                                  (m (DolanShimWit ground 'Negative (UVar name)))

mkPolarBisubstitution ::
       forall (ground :: GroundTypeKind) polarity m name. Is PolarityType polarity
    => SymbolType name
    -> m (DolanShimWit ground polarity (UVar name))
    -> m (DolanShimWit ground (InvertPolarity polarity) (UVar name))
    -> Bisubstitution ground m
mkPolarBisubstitution n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution n a b
        NegativeType -> MkBisubstitution n b a

getBisubstitution ::
       forall (polarity :: Polarity) (shim :: MapKind Type) m (wit :: Polarity -> Type -> Type) t.
       Is PolarityType polarity
    => m (PShimWit shim wit 'Positive t)
    -> m (PShimWit shim wit 'Negative t)
    -> m (PShimWit shim wit polarity t)
getBisubstitution tp tq =
    case polarityType @polarity of
        PositiveType -> tp
        NegativeType -> tq

bisubstituteSingularType ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, MonadOne m, Is PolarityType polarity)
    => Bisubstitution ground m
    -> DolanSingularType ground polarity t
    -> m (DolanShimWit ground polarity t)
bisubstituteSingularType (MkBisubstitution n tp tq) (VarDolanSingularType n')
    | Just Refl <- testEquality n n' = getBisubstitution tp tq
bisubstituteSingularType bisub t = do
    t' <- mapDolanSingularTypeM (bisubstituteType bisub) t
    return $ singleDolanShimWit t'

bisubstitutePlainType ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, MonadOne m, Is PolarityType polarity)
    => Bisubstitution ground m
    -> DolanPlainType ground polarity t
    -> m (DolanShimWit ground polarity t)
bisubstitutePlainType _ NilDolanPlainType = return $ mkShimWit $ PlainDolanType NilDolanPlainType
bisubstitutePlainType bisub (ConsDolanPlainType ta tb) = do
    tfa <- bisubstituteSingularType bisub ta
    tfb <- bisubstitutePlainType bisub tb
    return $ joinMeetDolanShimWit tfa tfb

bisubstitutePlainShimWit ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, MonadOne m, Is PolarityType polarity)
    => Bisubstitution ground m
    -> DolanPlainShimWit ground polarity t
    -> m (DolanShimWit ground polarity t)
bisubstitutePlainShimWit bisub = chainShimWitM $ bisubstitutePlainType bisub

renameBisubstitution ::
       forall (ground :: GroundTypeKind) m. (IsDolanGroundType ground, MonadOne m)
    => Bisubstitution ground m
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) ()
renameBisubstitution (MkBisubstitution _ mpos mneg) = do
    case getMaybeOne mpos of
        Just (MkShimWit bt _) -> do
            _ <- renameDolanType bt
            return ()
        Nothing -> return ()
    case getMaybeOne mneg of
        Just (MkShimWit bt _) -> do
            _ <- renameDolanType bt
            return ()
        Nothing -> return ()

bisubstituteType ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, MonadOne m, Is PolarityType polarity)
    => Bisubstitution ground m
    -> DolanType ground polarity t
    -> m (DolanShimWit ground polarity t)
bisubstituteType bisub (PlainDolanType pt) = bisubstitutePlainType bisub pt
bisubstituteType (MkBisubstitution nb _ _) t@(RecursiveDolanType nt _)
    | Just Refl <- testEquality nb nt = return $ mkShimWit t
bisubstituteType bisub rt@(RecursiveDolanType n pt) =
    runVarRenamerT $
    runVarNamespaceT $ do
        renameBisubstitution bisub
        _ <- renameDolanType rt
        varNamespaceTAddUVars @_ @_ @(DolanPolyShim ground Type) (ConsListType n NilListType) $ \n' _ -> do
            pt' <- renameDolanPlainType pt
            t' <- lift $ lift $ bisubstitutePlainShimWit bisub pt'
            return $ recursiveDolanShimWit n' t'

bisubstituteShimWit ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, MonadOne m, Is PolarityType polarity)
    => Bisubstitution ground m
    -> DolanShimWit ground polarity t
    -> m (DolanShimWit ground polarity t)
bisubstituteShimWit bisub = chainShimWitM $ bisubstituteType bisub

bisubstitutesType ::
       forall (ground :: GroundTypeKind) m polarity t. (IsDolanGroundType ground, MonadOne m, Is PolarityType polarity)
    => [Bisubstitution ground m]
    -> DolanType ground polarity t
    -> m (DolanShimWit ground polarity t)
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
    expr' <- mapPShimWitsM (bisubstituteType sub) (bisubstituteType sub) expr
    bisubstitutes subs expr'
