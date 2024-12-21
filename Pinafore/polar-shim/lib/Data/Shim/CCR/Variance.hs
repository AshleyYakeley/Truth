module Data.Shim.CCR.Variance where

import Data.Shim.Mono
import Data.Shim.Range
import Shapes

data CCRVariance
    = SimpleCCRVariance Variance
    | RangeCCRVariance

type CoCCRVariance = 'SimpleCCRVariance 'Covariance

type ContraCCRVariance = 'SimpleCCRVariance 'Contravariance

type family CCRVarianceKind (sv :: CCRVariance) :: Type where
    CCRVarianceKind ('SimpleCCRVariance _) = Type
    CCRVarianceKind 'RangeCCRVariance = (Type, Type)

data CCRVarianceType (t :: CCRVariance) where
    CoCCRVarianceType :: CCRVarianceType CoCCRVariance
    ContraCCRVarianceType :: CCRVarianceType ContraCCRVariance
    RangeCCRVarianceType :: CCRVarianceType 'RangeCCRVariance

instance TestEquality CCRVarianceType where
    testEquality CoCCRVarianceType CoCCRVarianceType = Just Refl
    testEquality ContraCCRVarianceType ContraCCRVarianceType = Just Refl
    testEquality RangeCCRVarianceType RangeCCRVarianceType = Just Refl
    testEquality _ _ = Nothing

instance Show (CCRVarianceType t) where
    show CoCCRVarianceType = "co"
    show ContraCCRVarianceType = "contra"
    show RangeCCRVarianceType = "range"

instance Representative CCRVarianceType where
    getRepWitness CoCCRVarianceType = Dict
    getRepWitness ContraCCRVarianceType = Dict
    getRepWitness RangeCCRVarianceType = Dict

instance Is VarianceType v => Is CCRVarianceType ('SimpleCCRVariance v) where
    representative =
        case representative @_ @VarianceType @v of
            CoVarianceType -> CoCCRVarianceType
            ContraVarianceType -> ContraCCRVarianceType

instance Is CCRVarianceType 'RangeCCRVariance where
    representative = RangeCCRVarianceType

ccrVarianceCoercibleKind :: forall (sv :: CCRVariance). CCRVarianceType sv -> Dict (CoercibleKind (CCRVarianceKind sv))
ccrVarianceCoercibleKind CoCCRVarianceType = Dict
ccrVarianceCoercibleKind ContraCCRVarianceType = Dict
ccrVarianceCoercibleKind RangeCCRVarianceType = Dict

type CCRVarianceCategory ::
       ShimKind Type -> forall (sv :: CCRVariance) -> CCRVarianceKind sv -> CCRVarianceKind sv -> Type
type family CCRVarianceCategory cat sv where
    CCRVarianceCategory cat ('SimpleCCRVariance v) = VarianceCategory cat v
    CCRVarianceCategory cat 'RangeCCRVariance = CatRange cat

ccrVarianceCategory ::
       forall cat (sv :: CCRVariance). Category cat
    => CCRVarianceType sv
    -> Dict (Category (CCRVarianceCategory cat sv))
ccrVarianceCategory CoCCRVarianceType = Dict
ccrVarianceCategory ContraCCRVarianceType = Dict
ccrVarianceCategory RangeCCRVarianceType = Dict

ccrVarianceCategoryShow ::
       forall cat (sv :: CCRVariance) a b. (forall p q. Show (cat p q))
    => CCRVarianceType sv
    -> CCRVarianceCategory cat sv a b
    -> String
ccrVarianceCategoryShow CoCCRVarianceType = show
ccrVarianceCategoryShow ContraCCRVarianceType = show
ccrVarianceCategoryShow RangeCCRVarianceType = show

type CCRVarianceMap (cat :: forall kc. kc -> kc -> Type) (sv :: CCRVariance) (f :: CCRVarianceKind sv -> k)
     = forall (a :: CCRVarianceKind sv) (b :: CCRVarianceKind sv). CCRVarianceCategory cat sv a b -> cat (f a) (f b)

data CCRVariation (sv :: CCRVariance) (f :: CCRVarianceKind sv -> k) = MkCCRVariation
    { ccrvMaybeRepresentational :: Maybe (Dict (RepresentationalRole f))
    , ccrvMap :: forall (a :: CCRVarianceKind sv) (b :: CCRVarianceKind sv).
                         CCRVarianceCategory KindFunction sv a b -> KindFunction (f a) (f b)
    }

class (MaybeRepresentational f, Is CCRVarianceType sv, CatFunctor (CCRVarianceCategory KindFunction sv) KindFunction f) =>
          HasCCRVariance (sv :: CCRVariance) (f :: CCRVarianceKind sv -> k)
    | f -> sv


ccrVariation ::
       forall (sv :: CCRVariance) k (f :: CCRVarianceKind sv -> k). HasCCRVariance sv f
    => CCRVariation sv f
ccrVariation = MkCCRVariation {ccrvMaybeRepresentational = maybeRepresentational, ccrvMap = cfmap}

instance forall v k (f :: Type -> k). (HasVariance f, VarianceOf f ~ v) => HasCCRVariance ('SimpleCCRVariance v) f
