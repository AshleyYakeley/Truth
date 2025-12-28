module Data.SpecialCase.Arrow
    ( ArrowOrConst
    , FunctorGetPure (..)
    , constFunctionAp
    )
where

import Data.CatFunctor
import Data.SpecialCase.Applicative
import Data.SpecialCase.SpecialCase
import Shapes.Import

newtype ArrowOrConst (cat :: Type -> Type -> Type) a b = MkArrowOrConst {unArrowOrConst :: ApplicativeOrConst (cat a) b}
    deriving newtype (Functor, Applicative)

instance Applicative (cat a) => HasSpecialCase "general" (ArrowOrConst cat a b) where
    type SpecialCase "general" (ArrowOrConst cat a b) = cat a b
    mkSpecial = MkArrowOrConst . General
    checkSpecial = Just . unGeneral . unArrowOrConst

instance Applicative (cat a) => HasSpecialCases (ArrowOrConst cat a b) where
    unGeneral = unGeneral . unArrowOrConst

instance Applicative (cat a) => HasSpecialCase "const" (ArrowOrConst cat a b) where
    type SpecialCase "const" (ArrowOrConst cat a b) = b
    mkSpecial = MkArrowOrConst . Special @"const"
    checkSpecial = checkSpecial @"const" . unArrowOrConst

type FunctionOrConst = ArrowOrConst (->)

instance Category FunctionOrConst where
    id = MkArrowOrConst $ General id
    cbc . cab = case cbc of
        Special @"const" c -> Special @"const" c
        General bc -> case cab of
            Special @"const" b -> Special @"const" $ bc b
            General ab -> General $ bc . ab

instance (forall x. Applicative (cat x), Category (ArrowOrConst cat), Arrow cat) => Arrow (ArrowOrConst cat) where
    arr f = General $ arr f
    first (General abc) = General $ first abc

class Functor f => FunctorGetPure f where
    getPure :: forall a b. FunctionOrConst (f a) (b -> f b)
    getPure = General $ \fa b -> fmap (const b) fa

instance FunctorGetPure ((->) p) where
    getPure = Special @"const" pure

instance FunctorGetPure f => CatFunctor FunctionOrConst FunctionOrConst f where
    cfmap = \case
        Special @"const" b -> fmap (\bfb -> bfb b) getPure
        General ab -> General $ fmap ab

instance FunctorGetPure Identity where
    getPure = Special @"const" pure

instance FunctorGetPure Maybe where
    getPure = Special @"const" pure

instance FunctorGetPure (Either p) where
    getPure = Special @"const" pure

instance FunctorGetPure ((,) p)

instance FunctorGetPure (Result e) where
    getPure = Special @"const" pure

constFunctionAp :: (MonadInner f, Applicative (t (f a)), CatFunctor t t f) => f (t a b) -> t (f a) (f b)
constFunctionAp fcab =
    case retrieveInner fcab of
        FailureResult e -> pure $ throwExc e
        SuccessResult cab -> cfmap cab
