module Language.Expression.TypeMappable where

import Language.Expression.Polarity
import Language.Expression.TypeF
import Shapes

class TypeMappable (cat :: k -> k -> Type) (poswit :: k -> Type) (negwit :: k -> Type) a where
    mapTypesM ::
           forall m. Monad m
        => (forall t. poswit t -> m (GenTypeF cat poswit 'Positive t))
        -> (forall t. negwit t -> m (GenTypeF cat negwit 'Negative t))
        -> a
        -> m a

mapTypes ::
       forall cat poswit negwit a. TypeMappable cat poswit negwit a
    => (forall t. poswit t -> GenTypeF cat poswit 'Positive t)
    -> (forall t. negwit t -> GenTypeF cat negwit 'Negative t)
    -> a
    -> a
mapTypes mapPos mapNeg a = runIdentity $ mapTypesM (\t -> Identity $ mapPos t) (\t -> Identity $ mapNeg t) a

mappableGetTypes ::
       forall k (cat :: k -> k -> Type) (poswit :: k -> Type) (negwit :: k -> Type) a.
       (Category cat, TypeMappable cat poswit negwit a)
    => a
    -> [Either (AnyW poswit) (AnyW negwit)]
mappableGetTypes a =
    execWriter $
    mapTypesM
        @cat
        (\t -> do
             tell $ pure $ Left $ MkAnyW t
             return $ mkGenTypeF t)
        (\t -> do
             tell $ pure $ Right $ MkAnyW t
             return $ mkGenTypeF t)
        a

instance Category cat => TypeMappable cat (poswit :: k -> Type) negwit (GenTypeF cat poswit 'Positive t) where
    mapTypesM mapPos _ = chainTypeFM mapPos

instance Category cat => TypeMappable cat poswit (negwit :: k -> Type) (GenTypeF cat negwit 'Negative t) where
    mapTypesM _ mapNeg = chainTypeFM mapNeg

instance TypeMappable cat poswit negwit (AnyW poswit) where
    mapTypesM mapPos _ (MkAnyW pa) = do
        MkTypeF t _ <- mapPos pa
        return $ MkAnyW t

instance TypeMappable cat poswit negwit (AnyW negwit) where
    mapTypesM _ mapNeg (MkAnyW pa) = do
        MkTypeF t _ <- mapNeg pa
        return $ MkAnyW t
