module Language.Expression.Dolan.TypeF where

import Language.Expression.Dolan.Polarity
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Sealed
import Shapes

data TypeF (wit :: TypePolarity -> k -> Type) (polarity :: TypePolarity) (t :: k) :: Type where
    MkTypeF
        :: forall (k :: Type) wit polarity (t :: k) (t' :: k).
           wit polarity t'
        -> ConvertType polarity t t'
        -> TypeF wit polarity t

mkTypeF ::
       forall (k :: Type) wit polarity (t :: k). (Category (KindMorphism k (->)), IsTypePolarity polarity)
    => wit polarity t
    -> TypeF wit polarity t
mkTypeF t =
    case whichTypePolarity @polarity of
        Left Refl -> MkTypeF t id
        Right Refl -> MkTypeF t id

unTypeF :: TypeF wit polarity t -> (forall t'. wit polarity t' -> ConvertType polarity t t' -> r) -> r
unTypeF (MkTypeF t conv) cont = cont t conv

toTypeFAnyValue :: TypeF wit 'PositivePolarity t -> t -> AnyValue (wit 'PositivePolarity)
toTypeFAnyValue (MkTypeF t conv) v = MkAnyValue t $ conv v

mapTypeF ::
       forall (k :: Type) wit polarity (a :: k) (b :: k). (Category (KindMorphism k (->)), IsTypePolarity polarity)
    => ConvertType polarity b a
    -> TypeF wit polarity a
    -> TypeF wit polarity b
mapTypeF ab (MkTypeF t conv) =
    MkTypeF t $
    case whichTypePolarity @polarity of
        Left Refl -> conv . ab
        Right Refl -> ab . conv

instance Contravariant (TypeF wit 'PositivePolarity) where
    contramap = mapTypeF

instance Functor (TypeF wit 'NegativePolarity) where
    fmap = mapTypeF

chainTypeFM ::
       forall m (k :: Type) (wita :: TypePolarity -> k -> Type) (witb :: TypePolarity -> k -> Type) polarity (t' :: k).
       (Monad m, Category (KindMorphism k (->)), IsTypePolarity polarity)
    => (forall (t :: k). wita polarity t -> m (TypeF witb polarity t))
    -> TypeF wita polarity t'
    -> m (TypeF witb polarity t')
chainTypeFM f (MkTypeF t conv) = do
    tf <- f t
    return $ mapTypeF conv tf

chainTypeF ::
       forall (k :: Type) (wita :: TypePolarity -> k -> Type) (witb :: TypePolarity -> k -> Type) polarity (t' :: k).
       (Category (KindMorphism k (->)), IsTypePolarity polarity)
    => (forall (t :: k). wita polarity t -> TypeF witb polarity t)
    -> TypeF wita polarity t'
    -> TypeF witb polarity t'
chainTypeF f (MkTypeF t conv) = mapTypeF conv $ f t

typeFConstExpression ::
       TypeF wit 'PositivePolarity t -> t -> SealedExpression name (wit 'NegativePolarity) (wit 'PositivePolarity)
typeFConstExpression (MkTypeF tt conv) t = MkSealedExpression tt $ pure $ conv t

class MapTypes wit a | a -> wit where
    mapTypesM ::
           forall m. Monad m
        => (forall t. wit 'PositivePolarity t -> m (TypeF wit 'PositivePolarity t))
        -> (forall t. wit 'NegativePolarity t -> m (TypeF wit 'NegativePolarity t))
        -> a
        -> m a

mapTypes ::
       MapTypes wit a
    => (forall t. wit 'PositivePolarity t -> TypeF wit 'PositivePolarity t)
    -> (forall t. wit 'NegativePolarity t -> TypeF wit 'NegativePolarity t)
    -> a
    -> a
mapTypes mapPos mapNeg a = runIdentity $ mapTypesM (\t -> Identity $ mapPos t) (\t -> Identity $ mapNeg t) a

instance MapTypes wit (NamedExpression name (wit 'NegativePolarity) a) where
    mapTypesM _ _ (ClosedExpression a) = return $ ClosedExpression a
    mapTypesM mapPos mapNeg (OpenExpression (MkNameWitness name tt) expr) = do
        MkTypeF tt' conv <- mapNeg tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ OpenExpression (MkNameWitness name tt') $ fmap (\ta -> ta . conv) expr'

instance MapTypes wit (SealedExpression name (wit 'NegativePolarity) (wit 'PositivePolarity)) where
    mapTypesM mapPos mapNeg (MkSealedExpression tt expr) = do
        MkTypeF tt' conv <- mapPos tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ MkSealedExpression tt' $ fmap conv expr'

instance MapTypes wit (NamedPattern name (wit 'PositivePolarity) a b) where
    mapTypesM _ _ (ClosedPattern a) = return $ ClosedPattern a
    mapTypesM mapPos mapNeg (OpenPattern (MkNameWitness name tt) pat) = do
        MkTypeF tt' conv <- mapPos tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ OpenPattern (MkNameWitness name tt') $ fmap (\(t, b) -> (conv t, b)) pat'

instance MapTypes wit (SealedPattern name (wit 'PositivePolarity) (wit 'NegativePolarity)) where
    mapTypesM mapPos mapNeg (MkSealedPattern tt pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ MkSealedPattern tt' $ pat' . arr conv

class ToTypeF wit t where
    toTypeF :: TypeF wit 'PositivePolarity t

class FromTypeF wit t where
    fromTypeF :: TypeF wit 'NegativePolarity t

toValue ::
       forall wit t. ToTypeF wit t
    => t
    -> AnyValue (wit 'PositivePolarity)
toValue = toTypeFAnyValue $ toTypeF @wit @t
