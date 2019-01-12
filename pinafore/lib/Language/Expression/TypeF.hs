module Language.Expression.TypeF where

import Language.Expression.Polarity
import Shapes

data TypeF (wit :: k -> Type) (polarity :: Polarity) (t :: k) :: Type where
    MkTypeF
        :: forall (k :: Type) wit polarity (t :: k) (t' :: k).
           wit t'
        -> ConvertType polarity t t'
        -> TypeF wit polarity t

mkTypeF ::
       forall polarity (k :: Type) wit (t :: k). (Category (KindMorphism k (->)), Is PolarityType polarity)
    => wit t
    -> TypeF wit polarity t
mkTypeF t =
    case representative @_ @_ @polarity of
        PositiveType -> MkTypeF t id
        NegativeType -> MkTypeF t id

unTypeF :: TypeF wit polarity t -> (forall t'. wit t' -> ConvertType polarity t t' -> r) -> r
unTypeF (MkTypeF t conv) cont = cont t conv

toTypeFAnyValue :: TypeF wit 'Positive t -> t -> AnyValue wit
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
       forall m (k :: Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (Monad m, Category (KindMorphism k (->)), Is PolarityType polarity)
    => (forall (t :: k). wita t -> m (TypeF witb polarity t))
    -> TypeF wita polarity t'
    -> m (TypeF witb polarity t')
chainTypeFM f (MkTypeF t conv) = do
    tf <- f t
    return $ mapTypeF conv tf

chainTypeF ::
       forall (k :: Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (Category (KindMorphism k (->)), Is PolarityType polarity)
    => (forall (t :: k). wita t -> TypeF witb polarity t)
    -> TypeF wita polarity t'
    -> TypeF witb polarity t'
chainTypeF f (MkTypeF t conv) = mapTypeF conv $ f t
