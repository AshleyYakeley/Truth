module Language.Expression.TypeF where

import Language.Expression.Polarity
import Shapes

data GenTypeF (cat :: k -> k -> Type) (wit :: k -> Type) (polarity :: Polarity) (t :: k) :: Type where
    MkTypeF
        :: forall (k :: Type) (cat :: k -> k -> Type) wit polarity (t :: k) (t' :: k).
           wit t'
        -> PolarMapType cat polarity t t'
        -> GenTypeF cat wit polarity t

--type ConvertType polarity (a :: k) (b :: k) = PolarMapType (KindMorphism k (->)) polarity a b
type TypeF (wit :: k -> Type) = GenTypeF (KindMorphism k (->)) wit

mkGenTypeF ::
       forall (k :: Type) (cat :: k -> k -> Type) polarity wit (t :: k). (Category cat, Is PolarityType polarity)
    => wit t
    -> GenTypeF cat wit polarity t
mkGenTypeF t =
    case representative @_ @_ @polarity of
        PositiveType -> MkTypeF t id
        NegativeType -> MkTypeF t id

mkTypeF ::
       forall (k :: Type) polarity wit (t :: k). (Category (KindMorphism k (->)), Is PolarityType polarity)
    => wit t
    -> TypeF wit polarity t
mkTypeF = mkGenTypeF

unTypeF :: GenTypeF cat wit polarity t -> (forall t'. wit t' -> PolarMapType cat polarity t t' -> r) -> r
unTypeF (MkTypeF t conv) cont = cont t conv

toTypeFAnyValue :: TypeF wit 'Positive t -> t -> AnyValue wit
toTypeFAnyValue (MkTypeF t conv) v = MkAnyValue t $ conv v

mapTypeF ::
       forall (k :: Type) (cat :: k -> k -> Type) wit polarity (a :: k) (b :: k).
       (Category cat, Is PolarityType polarity)
    => PolarMapType cat polarity b a
    -> GenTypeF cat wit polarity a
    -> GenTypeF cat wit polarity b
mapTypeF ab (MkTypeF t conv) =
    MkTypeF t $
    case representative @_ @_ @polarity of
        PositiveType -> conv . ab
        NegativeType -> ab . conv

instance Contravariant (GenTypeF (->) wit 'Positive) where
    contramap = mapTypeF

instance Functor (GenTypeF (->) wit 'Negative) where
    fmap = mapTypeF

chainTypeFM ::
       forall m (k :: Type) (cat :: k -> k -> Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (Monad m, Category cat, Is PolarityType polarity)
    => (forall (t :: k). wita t -> m (GenTypeF cat witb polarity t))
    -> GenTypeF cat wita polarity t'
    -> m (GenTypeF cat witb polarity t')
chainTypeFM f (MkTypeF t conv) = do
    tf <- f t
    return $ mapTypeF conv tf

chainTypeF ::
       forall (k :: Type) (cat :: k -> k -> Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (Category cat, Is PolarityType polarity)
    => (forall (t :: k). wita t -> GenTypeF cat witb polarity t)
    -> GenTypeF cat wita polarity t'
    -> GenTypeF cat witb polarity t'
chainTypeF f (MkTypeF t conv) = mapTypeF conv $ f t

hlistTypeF ::
       forall wit polarity lt. Is PolarityType polarity
    => ListType (TypeF wit polarity) lt
    -> TypeF (HListWit wit) polarity (HList lt)
hlistTypeF NilListType = mkGenTypeF $ MkHListWit NilListType
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
    toTypeF = mkGenTypeF (MkHListWit NilListType)

instance (ToTypeF wit t1, ToTypeF (HListWit wit) tr) => ToTypeF (HListWit wit) (t1, tr) where
    toTypeF =
        case (toTypeF @wit @t1, toTypeF @(HListWit wit) @tr) of
            (MkTypeF t1 conv1, MkTypeF (MkHListWit tr) convr) ->
                MkTypeF (MkHListWit $ ConsListType t1 tr) $ \(a1, ar) -> (conv1 a1, convr ar)
