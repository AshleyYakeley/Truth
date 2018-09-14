module Language.Expression.Dolan.TypeF where

import Language.Expression.Dolan.Polarity
import Language.Expression.Named
import Language.Expression.Sealed
import Shapes

data TypeF (wit :: TypePolarity -> Type -> Type) (polarity :: TypePolarity) (t :: Type) :: Type where
    MkTypeF :: wit polarity t' -> ConvertType polarity t t' -> TypeF wit polarity t

mkTypeF ::
       forall wit polarity t. IsTypePolarity polarity
    => wit polarity t
    -> TypeF wit polarity t
mkTypeF t =
    case whichTypePolarity @polarity of
        Left Refl -> MkTypeF t id
        Right Refl -> MkTypeF t id

unTypeF :: TypeF wit polarity t -> (forall t'. wit polarity t' -> ConvertType polarity t t' -> r) -> r
unTypeF (MkTypeF t conv) cont = cont t conv

instance Functor (TypeF wit 'NegativePolarity) where
    fmap ab (MkTypeF t conv) = MkTypeF t $ ab . conv

instance Contravariant (TypeF wit 'PositivePolarity) where
    contramap ab (MkTypeF t conv) = MkTypeF t $ conv . ab

typeFSealedExpression ::
       TypeF wit 'PositivePolarity t
    -> NamedExpression name (wit 'NegativePolarity) t
    -> SealedExpression name (wit 'NegativePolarity) (wit 'PositivePolarity)
typeFSealedExpression (MkTypeF tt conv) expr = MkSealedExpression tt $ fmap conv expr

typeFConstExpression ::
       TypeF wit 'PositivePolarity t -> t -> SealedExpression name (wit 'NegativePolarity) (wit 'PositivePolarity)
typeFConstExpression tf t = typeFSealedExpression tf $ pure t
