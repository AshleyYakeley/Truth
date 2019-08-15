module Data.Shim.Variance where

import Data.Shim.CatRange
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

instance TestEquality VarianceType where
    testEquality CovarianceType CovarianceType = Just Refl
    testEquality ContravarianceType ContravarianceType = Just Refl
    testEquality RangevarianceType RangevarianceType = Just Refl
    testEquality _ _ = Nothing

instance Show (VarianceType t) where
    show CovarianceType = "co"
    show ContravarianceType = "contra"
    show RangevarianceType = "range"

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

varianceCoercibleKind :: forall (v :: Variance). VarianceType v -> Dict (CoercibleKind (VarianceKind v))
varianceCoercibleKind CovarianceType = Dict
varianceCoercibleKind ContravarianceType = Dict
varianceCoercibleKind RangevarianceType = Dict

type family VarianceCategory (cat :: Type -> Type -> Type) (v :: Variance) :: VarianceKind v -> VarianceKind v -> Type where
    VarianceCategory cat 'Covariance = cat
    VarianceCategory cat 'Contravariance = CatDual cat
    VarianceCategory cat 'Rangevariance = CatRange cat

varianceInCategory ::
       forall cat (v :: Variance). InCategory cat
    => VarianceType v
    -> Dict (InCategory (VarianceCategory cat v))
varianceInCategory CovarianceType = Dict
varianceInCategory ContravarianceType = Dict
varianceInCategory RangevarianceType = Dict

varianceCategoryShow ::
       forall cat (v :: Variance) a b. (forall p q. Show (cat p q))
    => VarianceType v
    -> VarianceCategory cat v a b
    -> String
varianceCategoryShow CovarianceType = show
varianceCategoryShow ContravarianceType = show
varianceCategoryShow RangevarianceType = show

type VarianceMap (cat :: forall kc. kc -> kc -> Type) (v :: Variance) (f :: VarianceKind v -> k)
     = forall (a :: VarianceKind v) (b :: VarianceKind v).
           (InKind a, InKind b) => VarianceCategory cat v a b -> cat (f a) (f b)

class (InKind f, Is VarianceType v, CatFunctor (VarianceCategory KindFunction v) KindFunction f) =>
          HasVariance (v :: Variance) (f :: VarianceKind v -> k)
    | f -> v
    where
    varianceRepresentational :: Maybe (Dict (RepresentationalRole f))

instance HasVariance 'Covariance Maybe where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance [] where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance ((->) a) where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance ((,) a) where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance (Either a) where
    varianceRepresentational = Just Dict

instance HasVariance 'Contravariance (->) where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance (,) where
    varianceRepresentational = Just Dict

instance HasVariance 'Covariance Either where
    varianceRepresentational = Just Dict

type InVarianceKind sv (a :: VarianceKind sv) = InKind a
