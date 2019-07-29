module Data.Shim.Variance where

import Data.Shim.Range
import Shapes

data Variance
    = Covariance
    | Contravariance
    | Rangevariance

type family VarianceKind (v :: Variance) :: Type where
    VarianceKind 'Covariance = Type
    VarianceKind 'Contravariance = Type
    VarianceKind 'Rangevariance = (Type, Type)

data VarianceType (t :: Variance) where
    CovarianceType :: VarianceType 'Covariance
    ContravarianceType :: VarianceType 'Contravariance
    RangevarianceType :: VarianceType 'Rangevariance

instance Representative VarianceType where
    getRepWitness CovarianceType = Dict
    getRepWitness ContravarianceType = Dict
    getRepWitness RangevarianceType = Dict

instance Is VarianceType 'Covariance where
    representative = CovarianceType

instance Is VarianceType 'Contravariance where
    representative = ContravarianceType

instance Is VarianceType 'Rangevariance where
    representative = RangevarianceType

singleVarianceCoercibleKind :: forall (v :: Variance). VarianceType v -> Dict (CoercibleKind (VarianceKind v))
singleVarianceCoercibleKind CovarianceType = Dict
singleVarianceCoercibleKind ContravarianceType = Dict
singleVarianceCoercibleKind RangevarianceType = Dict

type family VarianceCategory (cat :: Type -> Type -> Type) (v :: Variance) :: VarianceKind v -> VarianceKind v -> Type where
    VarianceCategory cat 'Covariance = cat
    VarianceCategory cat 'Contravariance = CatDual cat
    VarianceCategory cat 'Rangevariance = CatRange cat

type VarianceMap (cat :: forall kc. kc -> kc -> Type) (v :: Variance) (gt :: VarianceKind v -> k)
     = forall (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b) => VarianceCategory cat v a b -> cat (gt a) (gt b)

mkRangevary ::
       forall k (cat :: forall kc. kc -> kc -> Type) (f :: (Type, Type) -> k). Category (cat :: Type -> Type -> Type)
    => (forall a b. (forall t. Range cat t a -> Range cat t b) -> cat (f a) (f b))
    -> VarianceMap cat 'Rangevariance f
mkRangevary f (MkCatRange pbpa qaqb) = f $ \(MkRange pt tq) -> MkRange (pt . pbpa) (qaqb . tq)

type InVarianceKind sv (a :: VarianceKind sv) = InKind a
