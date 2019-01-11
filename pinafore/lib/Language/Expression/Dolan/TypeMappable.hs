module Language.Expression.Dolan.TypeMappable where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Polarity
import Language.Expression.Sealed
import Language.Expression.TypeF
import Shapes

class TypeMappable wit a | a -> wit where
    mapTypesM ::
           forall m. Monad m
        => (forall t. wit 'Positive t -> m (TypeF wit 'Positive t))
        -> (forall t. wit 'Negative t -> m (TypeF wit 'Negative t))
        -> a
        -> m a

mapTypes ::
       TypeMappable wit a
    => (forall t. wit 'Positive t -> TypeF wit 'Positive t)
    -> (forall t. wit 'Negative t -> TypeF wit 'Negative t)
    -> a
    -> a
mapTypes mapPos mapNeg a = runIdentity $ mapTypesM (\t -> Identity $ mapPos t) (\t -> Identity $ mapNeg t) a

mappableGetTypes ::
       forall k (wit :: Polarity -> k -> Type) a. (Category (KindMorphism k (->)), TypeMappable wit a)
    => a
    -> [Either (AnyW (wit 'Positive)) (AnyW (wit 'Negative))]
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

instance (Is PolarityType polarity, Category (KindMorphism k (->))) =>
             TypeMappable (wit :: Polarity -> k -> Type) (TypeF wit polarity t) where
    mapTypesM mapPos mapNeg tf =
        case representative @_ @_ @polarity of
            PositiveType -> chainTypeFM mapPos tf
            NegativeType -> chainTypeFM mapNeg tf

instance TypeMappable wit (NamedExpression name (wit 'Negative) a) where
    mapTypesM _ _ (ClosedExpression a) = return $ ClosedExpression a
    mapTypesM mapPos mapNeg (OpenExpression (MkNameWitness name tt) expr) = do
        MkTypeF tt' conv <- mapNeg tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ OpenExpression (MkNameWitness name tt') $ fmap (\ta -> ta . conv) expr'

instance TypeMappable wit (SealedExpression name (wit 'Negative) (wit 'Positive)) where
    mapTypesM mapPos mapNeg (MkSealedExpression tt expr) = do
        MkTypeF tt' conv <- mapPos tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ MkSealedExpression tt' $ fmap conv expr'

instance TypeMappable wit (NamedPattern name (wit 'Positive) a b) where
    mapTypesM _ _ (ClosedPattern a) = return $ ClosedPattern a
    mapTypesM mapPos mapNeg (OpenPattern (MkNameWitness name tt) pat) = do
        MkTypeF tt' conv <- mapPos tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ OpenPattern (MkNameWitness name tt') $ fmap (\(t, b) -> (conv t, b)) pat'

instance TypeMappable wit (SealedPattern name (wit 'Positive) (wit 'Negative)) where
    mapTypesM mapPos mapNeg (MkSealedPattern tt pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ MkSealedPattern tt' $ pat' . arr conv

data HListPolWit wit polarity t where
    MkHListPolWit :: ListType (wit polarity) t -> HListPolWit wit polarity (HList t)

liftHListPolwit ::
       forall m wit polarity. (Is PolarityType polarity, Applicative m)
    => (forall t. wit polarity t -> m (TypeF wit polarity t))
    -> forall t'. HListPolWit wit polarity t' -> m (TypeF (HListPolWit wit) polarity t')
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

instance TypeMappable wit (PatternConstructor name (wit 'Positive) (wit 'Negative)) where
    mapTypesM mapPos mapNeg (MkPatternConstructor tt lvw pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        MkTypeF (MkHListPolWit lvw') lconv <-
            mapTypesM (liftHListPolwit mapPos) (liftHListPolwit mapNeg) $ mkTypeF $ MkHListPolWit lvw
        return $ MkPatternConstructor tt' lvw' $ fmap lconv $ pat' . arr conv
