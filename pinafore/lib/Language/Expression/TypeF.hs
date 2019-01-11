module Language.Expression.TypeF where

import Language.Expression.Polarity
import Language.Expression.Sealed
import Shapes

data TypeF (wit :: Polarity -> k -> Type) (polarity :: Polarity) (t :: k) :: Type where
    MkTypeF
        :: forall (k :: Type) wit polarity (t :: k) (t' :: k).
           wit polarity t'
        -> ConvertType polarity t t'
        -> TypeF wit polarity t

mkTypeF ::
       forall (k :: Type) wit polarity (t :: k). (Category (KindMorphism k (->)), Is PolarityType polarity)
    => wit polarity t
    -> TypeF wit polarity t
mkTypeF t =
    case representative @_ @_ @polarity of
        PositiveType -> MkTypeF t id
        NegativeType -> MkTypeF t id

unTypeF :: TypeF wit polarity t -> (forall t'. wit polarity t' -> ConvertType polarity t t' -> r) -> r
unTypeF (MkTypeF t conv) cont = cont t conv

toTypeFAnyValue :: TypeF wit 'Positive t -> t -> AnyValue (wit 'Positive)
toTypeFAnyValue (MkTypeF t conv) v = MkAnyValue t $ conv v

mapTypeF ::
       forall (k :: Type) wit polarity (a :: k) (b :: k). (Category (KindMorphism k (->)), Is PolarityType polarity)
    => ConvertType polarity b a
    -> TypeF wit polarity a
    -> TypeF wit polarity b
mapTypeF ab (MkTypeF t conv) =
    MkTypeF t $
    case representative @_ @_ @polarity of
        PositiveType -> conv . ab
        NegativeType -> ab . conv

instance Contravariant (TypeF wit 'Positive) where
    contramap = mapTypeF

instance Functor (TypeF wit 'Negative) where
    fmap = mapTypeF

chainTypeFM ::
       forall m (k :: Type) (wita :: Polarity -> k -> Type) (witb :: Polarity -> k -> Type) polarity (t' :: k).
       (Monad m, Category (KindMorphism k (->)), Is PolarityType polarity)
    => (forall (t :: k). wita polarity t -> m (TypeF witb polarity t))
    -> TypeF wita polarity t'
    -> m (TypeF witb polarity t')
chainTypeFM f (MkTypeF t conv) = do
    tf <- f t
    return $ mapTypeF conv tf

chainTypeF ::
       forall (k :: Type) (wita :: Polarity -> k -> Type) (witb :: Polarity -> k -> Type) polarity (t' :: k).
       (Category (KindMorphism k (->)), Is PolarityType polarity)
    => (forall (t :: k). wita polarity t -> TypeF witb polarity t)
    -> TypeF wita polarity t'
    -> TypeF witb polarity t'
chainTypeF f (MkTypeF t conv) = mapTypeF conv $ f t

typeFConstExpression :: TypeF wit 'Positive t -> t -> SealedExpression name (wit 'Negative) (wit 'Positive)
typeFConstExpression (MkTypeF tt conv) t = MkSealedExpression tt $ pure $ conv t

class ToTypeF wit t where
    toTypeF :: TypeF wit 'Positive t

class FromTypeF wit t where
    fromTypeF :: TypeF wit 'Negative t

toValue ::
       forall wit t. ToTypeF wit t
    => t
    -> AnyValue (wit 'Positive)
toValue = toTypeFAnyValue $ toTypeF @wit @t
