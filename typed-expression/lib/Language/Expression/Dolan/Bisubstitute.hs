module Language.Expression.Dolan.Bisubstitute
    ( Bisubstitution(..)
    , mkPolarBisubstitution
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
import Language.Expression.TypeVariable
import Shapes

type Bisubstitution :: GroundTypeKind -> (Type -> Type) -> Type
data Bisubstitution ground m =
    forall name. MkBisubstitution (SymbolType name)
                                  (m (DolanShimWit ground 'Positive (UVar Type name)))
                                  (m (DolanShimWit ground 'Negative (UVar Type name)))

mkPolarBisubstitution ::
       forall (ground :: GroundTypeKind) polarity m name. Is PolarityType polarity
    => SymbolType name
    -> m (DolanShimWit ground polarity (UVar Type name))
    -> m (DolanShimWit ground (InvertPolarity polarity) (UVar Type name))
    -> Bisubstitution ground m
mkPolarBisubstitution n a b =
    case polarityType @polarity of
        PositiveType -> MkBisubstitution n a b
        NegativeType -> MkBisubstitution n b a

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
        -> m (DolanShimWit ground polarity t)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity) (w :: Polarity -> Type -> Type) (shim :: ShimKind Type). ( Bisubstitutable ground polarity (w polarity)
         , Is PolarityType polarity
         , shim ~ DolanPolyShim ground Type
         , InCategory shim
         ) => Bisubstitutable ground polarity (PShimWit shim w polarity) where
    bisubstituteType wt = chainShimWitM $ bisubstituteType wt

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanSingularType ground polarity) where
    bisubstituteType (MkBisubstitution n tp tq) (VarDolanSingularType n')
        | Just Refl <- testEquality n n' = getBisubstitution tp tq
    bisubstituteType bisub t = do
        t' <- mapDolanSingularTypeM (bisubstituteType bisub) t
        return $ singleDolanShimWit t'

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanPlainType ground polarity) where
    bisubstituteType _ NilDolanPlainType = return $ mkShimWit $ PlainDolanType NilDolanPlainType
    bisubstituteType bisub (ConsDolanPlainType ta tb) = do
        tfa <- bisubstituteType bisub ta
        tfb <- bisubstituteType bisub tb
        return $ joinMeetShimWit tfa tfb

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

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Bisubstitutable ground polarity (DolanType ground polarity) where
    bisubstituteType bisub (PlainDolanType pt) = bisubstituteType bisub pt
    bisubstituteType (MkBisubstitution nb _ _) t@(RecursiveDolanType nt _)
        | Just Refl <- testEquality nb nt = return $ mkShimWit t
    bisubstituteType bisub rt@(RecursiveDolanType n pt) =
        runVarRenamerT $
        runVarNamespaceT $ do
            renameBisubstitution bisub
            _ <- renameDolanType rt
            newname <- varNamespaceTAddNames [uVarName n]
            pt' <- renameDolanPlainType pt
            t' <- lift $ lift $ bisubstituteType bisub pt'
            return $ recursiveDolanShimWit newname t'

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
