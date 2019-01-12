module Language.Expression.TypeMappable where

import Language.Expression.Polarity
import Language.Expression.TypeF
import Shapes

class TypeMappable (poswit :: k -> Type) (negwit :: k -> Type) a where
    mapTypesM ::
           forall m. Monad m
        => (forall t. poswit t -> m (TypeF poswit 'Positive t))
        -> (forall t. negwit t -> m (TypeF negwit 'Negative t))
        -> a
        -> m a

mapTypes ::
       forall poswit negwit a. TypeMappable poswit negwit a
    => (forall t. poswit t -> TypeF poswit 'Positive t)
    -> (forall t. negwit t -> TypeF negwit 'Negative t)
    -> a
    -> a
mapTypes mapPos mapNeg a = runIdentity $ mapTypesM (\t -> Identity $ mapPos t) (\t -> Identity $ mapNeg t) a

mappableGetTypes ::
       forall k (poswit :: k -> Type) (negwit :: k -> Type) a.
       (Category (KindMorphism k (->)), TypeMappable poswit negwit a)
    => a
    -> [Either (AnyW poswit) (AnyW negwit)]
mappableGetTypes a =
    execWriter $
    mapTypesM
        (\t -> do
             tell $ pure $ Left $ MkAnyW t
             return $ mkTypeF t)
        (\t -> do
             tell $ pure $ Right $ MkAnyW t
             return $ mkTypeF t)
        a

instance (Category (KindMorphism k (->))) => TypeMappable (poswit :: k -> Type) negwit (TypeF poswit 'Positive t) where
    mapTypesM mapPos _ = chainTypeFM mapPos

instance (Category (KindMorphism k (->))) => TypeMappable poswit (negwit :: k -> Type) (TypeF negwit 'Negative t) where
    mapTypesM _ mapNeg = chainTypeFM mapNeg
