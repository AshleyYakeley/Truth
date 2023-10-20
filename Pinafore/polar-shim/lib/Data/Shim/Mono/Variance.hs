module Data.Shim.Mono.Variance where

import Shapes

data Variance
    = Covariance
    | Contravariance

data VarianceType (t :: Variance) where
    CoVarianceType :: VarianceType 'Covariance
    ContraVarianceType :: VarianceType 'Contravariance

invertVarianceType :: VarianceType t -> (forall v'. VarianceType v' -> r) -> r
invertVarianceType CoVarianceType call = call ContraVarianceType
invertVarianceType ContraVarianceType call = call CoVarianceType

instance TestEquality VarianceType where
    testEquality CoVarianceType CoVarianceType = Just Refl
    testEquality ContraVarianceType ContraVarianceType = Just Refl
    testEquality _ _ = Nothing

instance Show (VarianceType t) where
    show CoVarianceType = "co"
    show ContraVarianceType = "contra"

instance Representative VarianceType where
    getRepWitness CoVarianceType = Dict
    getRepWitness ContraVarianceType = Dict

instance Is VarianceType 'Covariance where
    representative = CoVarianceType

instance Is VarianceType 'Contravariance where
    representative = ContraVarianceType

type VarianceCategory :: (Type -> Type -> Type) -> Variance -> Type -> Type -> Type
type family VarianceCategory cat v where
    VarianceCategory cat 'Covariance = cat
    VarianceCategory cat 'Contravariance = CatDual cat

class ( MaybeRepresentational f
      , Is VarianceType (VarianceOf f)
      , CatFunctor (VarianceCategory KindFunction (VarianceOf f)) KindFunction f
      ) => HasVariance (f :: Type -> k) where
    type VarianceOf f :: Variance

instance HasVariance Maybe where
    type VarianceOf Maybe = 'Covariance

instance HasVariance [] where
    type VarianceOf [] = 'Covariance

instance HasVariance NonEmpty where
    type VarianceOf NonEmpty = 'Covariance

instance HasVariance (Map k) where
    type VarianceOf (Map k) = 'Covariance

instance HasVariance ((->) a) where
    type VarianceOf ((->) a) = 'Covariance

instance HasVariance ((,) a) where
    type VarianceOf ((,) a) = 'Covariance

instance HasVariance (Either a) where
    type VarianceOf (Either a) = 'Covariance

instance HasVariance (->) where
    type VarianceOf (->) = 'Contravariance

instance HasVariance (,) where
    type VarianceOf (,) = 'Covariance

instance HasVariance Either where
    type VarianceOf Either = 'Covariance

instance HasVariance Vector where
    type VarianceOf Vector = 'Covariance
