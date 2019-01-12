module Language.Expression.TypeMappable where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Polarity
import Language.Expression.Sealed
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

instance TypeMappable poswit negwit (NamedExpression name negwit a) where
    mapTypesM _ _ (ClosedExpression a) = return $ ClosedExpression a
    mapTypesM mapPos mapNeg (OpenExpression (MkNameWitness name tt) expr) = do
        MkTypeF tt' conv <- mapNeg tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ OpenExpression (MkNameWitness name tt') $ fmap (\ta -> ta . conv) expr'

instance TypeMappable poswit negwit (SealedExpression name negwit poswit) where
    mapTypesM mapPos mapNeg (MkSealedExpression tt expr) = do
        MkTypeF tt' conv <- mapPos tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ MkSealedExpression tt' $ fmap conv expr'

instance TypeMappable poswit negwit (NamedPattern name poswit a b) where
    mapTypesM _ _ (ClosedPattern a) = return $ ClosedPattern a
    mapTypesM mapPos mapNeg (OpenPattern (MkNameWitness name tt) pat) = do
        MkTypeF tt' conv <- mapPos tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ OpenPattern (MkNameWitness name tt') $ fmap (\(t, b) -> (conv t, b)) pat'

instance TypeMappable poswit negwit (SealedPattern name poswit negwit) where
    mapTypesM mapPos mapNeg (MkSealedPattern tt pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ MkSealedPattern tt' $ pat' . arr conv

data HListPolWit wit t where
    MkHListPolWit :: ListType wit t -> HListPolWit wit (HList t)

liftHListPolwit ::
       forall m wit polarity. (Is PolarityType polarity, Applicative m)
    => (forall t. wit t -> m (TypeF wit polarity t))
    -> forall t'. HListPolWit wit t' -> m (TypeF (HListPolWit wit) polarity t')
liftHListPolwit _ff (MkHListPolWit NilListType) = pure $ mkTypeF $ MkHListPolWit NilListType
liftHListPolwit ff (MkHListPolWit (ConsListType t tt)) = let
    combineTFs ::
           forall a lt.
           TypeF wit polarity a
        -> TypeF (HListPolWit wit) polarity (HList lt)
        -> TypeF (HListPolWit wit) polarity (a, HList lt)
    combineTFs (MkTypeF w1 conv1) (MkTypeF (MkHListPolWit wr) convr) =
        MkTypeF (MkHListPolWit $ ConsListType w1 wr) $
        case representative @_ @_ @polarity of
            PositiveType -> \(a1, ar) -> (conv1 a1, convr ar)
            NegativeType -> \(a1, ar) -> (conv1 a1, convr ar)
    in combineTFs <$> ff t <*> liftHListPolwit ff (MkHListPolWit tt)

instance TypeMappable poswit negwit (PatternConstructor name poswit negwit) where
    mapTypesM mapPos mapNeg (MkPatternConstructor tt lvw pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        MkTypeF (MkHListPolWit lvw') lconv <-
            mapTypesM (liftHListPolwit mapPos) mapNeg $ mkTypeF @'Positive $ MkHListPolWit lvw
        return $ MkPatternConstructor tt' lvw' $ fmap lconv $ pat' . arr conv
