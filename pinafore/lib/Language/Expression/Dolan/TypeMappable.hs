module Language.Expression.Dolan.TypeMappable where

import Language.Expression.Dolan.Polarity
import Language.Expression.Dolan.TypeF
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Sealed
import Shapes

class TypeMappable wit a | a -> wit where
    mapTypesM ::
           forall m. Monad m
        => (forall t. wit 'PositivePolarity t -> m (TypeF wit 'PositivePolarity t))
        -> (forall t. wit 'NegativePolarity t -> m (TypeF wit 'NegativePolarity t))
        -> a
        -> m a

mapTypes ::
       TypeMappable wit a
    => (forall t. wit 'PositivePolarity t -> TypeF wit 'PositivePolarity t)
    -> (forall t. wit 'NegativePolarity t -> TypeF wit 'NegativePolarity t)
    -> a
    -> a
mapTypes mapPos mapNeg a = runIdentity $ mapTypesM (\t -> Identity $ mapPos t) (\t -> Identity $ mapNeg t) a

mappableGetTypes ::
       forall k (wit :: TypePolarity -> k -> Type) a. (Category (KindMorphism k (->)), TypeMappable wit a)
    => a
    -> [Either (AnyW (wit 'PositivePolarity)) (AnyW (wit 'NegativePolarity))]
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

instance (IsTypePolarity polarity, Category (KindMorphism k (->))) =>
             TypeMappable (wit :: TypePolarity -> k -> Type) (TypeF wit polarity t) where
    mapTypesM mapPos mapNeg tf =
        case whichTypePolarity @polarity of
            Left Refl -> chainTypeFM mapPos tf
            Right Refl -> chainTypeFM mapNeg tf

instance TypeMappable wit (NamedExpression name (wit 'NegativePolarity) a) where
    mapTypesM _ _ (ClosedExpression a) = return $ ClosedExpression a
    mapTypesM mapPos mapNeg (OpenExpression (MkNameWitness name tt) expr) = do
        MkTypeF tt' conv <- mapNeg tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ OpenExpression (MkNameWitness name tt') $ fmap (\ta -> ta . conv) expr'

instance TypeMappable wit (SealedExpression name (wit 'NegativePolarity) (wit 'PositivePolarity)) where
    mapTypesM mapPos mapNeg (MkSealedExpression tt expr) = do
        MkTypeF tt' conv <- mapPos tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ MkSealedExpression tt' $ fmap conv expr'

instance TypeMappable wit (NamedPattern name (wit 'PositivePolarity) a b) where
    mapTypesM _ _ (ClosedPattern a) = return $ ClosedPattern a
    mapTypesM mapPos mapNeg (OpenPattern (MkNameWitness name tt) pat) = do
        MkTypeF tt' conv <- mapPos tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ OpenPattern (MkNameWitness name tt') $ fmap (\(t, b) -> (conv t, b)) pat'

instance TypeMappable wit (SealedPattern name (wit 'PositivePolarity) (wit 'NegativePolarity)) where
    mapTypesM mapPos mapNeg (MkSealedPattern tt pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ MkSealedPattern tt' $ pat' . arr conv

data HListPolWit wit polarity t where
    MkHListPolWit :: ListType (wit polarity) t -> HListPolWit wit polarity (HList t)

liftHListPolwit ::
       forall m wit polarity. (IsTypePolarity polarity, Applicative m)
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
        case whichTypePolarity @polarity of
            Left Refl -> \(a1, ar) -> (conv1 a1, convr ar)
            Right Refl -> \(a1, ar) -> (conv1 a1, convr ar)
    in combineTFs <$> ff t <*> liftHListPolwit ff (MkHListPolWit tt)

instance TypeMappable wit (PatternConstructor name (wit 'PositivePolarity) (wit 'NegativePolarity)) where
    mapTypesM mapPos mapNeg (MkPatternConstructor tt lvw pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        MkTypeF (MkHListPolWit lvw') lconv <-
            mapTypesM (liftHListPolwit mapPos) (liftHListPolwit mapNeg) $ mkTypeF $ MkHListPolWit lvw
        return $ MkPatternConstructor tt' lvw' $ fmap lconv $ pat' . arr conv
