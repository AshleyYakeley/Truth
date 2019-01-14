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

hlistTypeF ::
       forall wit polarity lt. Is PolarityType polarity
    => ListType (TypeF wit polarity) lt
    -> TypeF (HListWit wit) polarity (HList lt)
hlistTypeF NilListType = mkTypeF $ MkHListWit NilListType
hlistTypeF (ConsListType (MkTypeF t conv1) tt) =
    case hlistTypeF tt of
        MkTypeF (MkHListWit tt') convr ->
            MkTypeF (MkHListWit $ ConsListType t tt') $
            case representative @_ @_ @polarity of
                PositiveType -> \(a1, ar) -> (conv1 a1, convr ar)
                NegativeType -> \(a1, ar) -> (conv1 a1, convr ar)

class ToTypeF wit t where
    toTypeF :: TypeF wit 'Positive t

class FromTypeF wit t where
    fromTypeF :: TypeF wit 'Negative t

toValue ::
       forall wit t. ToTypeF wit t
    => t
    -> AnyValue wit
toValue = toTypeFAnyValue $ toTypeF @wit @t

instance ToTypeF (HListWit wit) () where
    toTypeF = mkTypeF (MkHListWit NilListType)

instance (ToTypeF wit t1, ToTypeF (HListWit wit) tr) => ToTypeF (HListWit wit) (t1, tr) where
    toTypeF =
        case (toTypeF @wit @t1, toTypeF @(HListWit wit) @tr) of
            (MkTypeF t1 conv1, MkTypeF (MkHListWit tr) convr) ->
                MkTypeF (MkHListWit $ ConsListType t1 tr) $ \(a1, ar) -> (conv1 a1, convr ar)
