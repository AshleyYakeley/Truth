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

instance (forall a. Applicative (cat a), Category cat) => Category (ArrowOrConst cat) where
    id = MkArrowOrConst $ General id
    cbc . cab = case cbc of
        Special @"const" c -> Special @"const" c
        General bc -> General $ bc . unGeneral cab

instance (forall a. Applicative (cat a), Arrow cat) => Arrow (ArrowOrConst cat) where
    arr f = General $ arr f
    first abc = General $ first (unGeneral abc)

type FunctionOrConst = ArrowOrConst (->)

class Functor f => FunctorGetPure f where
    getPure :: forall a b. FunctionOrConst (f a) (b -> f b)
    getPure = General $ \fa b -> fmap (const b) fa

applicativeGetPure :: Applicative f => FunctionOrConst (f a) (b -> f b)
applicativeGetPure = mkSpecial @"const" pure

instance FunctorGetPure ((->) p) where
    getPure = applicativeGetPure

instance FunctorGetPure f => CatFunctor FunctionOrConst FunctionOrConst f where
    cfmap = \case
        Special @"const" b -> fmap (\bfb -> bfb b) getPure
        General ab -> General $ fmap ab

instance FunctorGetPure Identity where
    getPure = applicativeGetPure

instance FunctorGetPure Maybe where
    getPure = applicativeGetPure

instance FunctorGetPure (Either p) where
    getPure = applicativeGetPure

instance FunctorGetPure ((,) p)

instance FunctorGetPure (Result e) where
    getPure = applicativeGetPure

constFunctionAp :: (MonadInner f, Applicative (t (f a)), CatFunctor t t f) => f (t a b) -> t (f a) (f b)
constFunctionAp fcab =
    case retrieveInner fcab of
        FailureResult e -> pure $ throwExc e
        SuccessResult cab -> cfmap cab
