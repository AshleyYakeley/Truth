module Language.Expression.Dolan.TypeF where

import Language.Expression.Dolan.Polarity
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

class ToTypeF wit t where
    toTypeF :: TypeF wit 'PositivePolarity t

class FromTypeF wit t where
    fromTypeF :: TypeF wit 'NegativePolarity t

toValue ::
       forall wit t. ToTypeF wit t
    => t
    -> AnyValue (wit 'PositivePolarity)
toValue = toTypeFAnyValue $ toTypeF @wit @t
