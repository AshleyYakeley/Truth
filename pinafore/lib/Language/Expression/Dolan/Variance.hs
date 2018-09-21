module Language.Expression.Dolan.Variance where

import Language.Expression.Dolan.TypeRange
import Shapes

data SingleVariance
    = Covariance
    | Contravariance
    | Rangevariance

type family SingleVarianceKind (v :: SingleVariance) :: Type where
    SingleVarianceKind 'Covariance = Type
    SingleVarianceKind 'Contravariance = Type
    SingleVarianceKind 'Rangevariance = (Type, Type)

data SingleVarianceType (t :: SingleVariance) where
    CovarianceType :: SingleVarianceType 'Covariance
    ContravarianceType :: SingleVarianceType 'Contravariance
    RangevarianceType :: SingleVarianceType 'Rangevariance

instance Representative SingleVarianceType where
    getRepWitness CovarianceType = Dict
    getRepWitness ContravarianceType = Dict
    getRepWitness RangevarianceType = Dict

instance Is SingleVarianceType 'Covariance where
    representative = CovarianceType

instance Is SingleVarianceType 'Contravariance where
    representative = ContravarianceType

instance Is SingleVarianceType 'Rangevariance where
    representative = RangevarianceType

data RangeFunc (a :: (Type, Type)) (b :: (Type, Type)) where
    MkRangeFunc :: forall pa qa pb qb. (pb -> pa) -> (qa -> qb) -> RangeFunc '( pa, qa) '( pb, qb)

type family SingleVarianceFunc (v :: SingleVariance) :: SingleVarianceKind v -> SingleVarianceKind v -> Type where
    SingleVarianceFunc 'Covariance = (->)
    SingleVarianceFunc 'Contravariance = CatDual (->)
    SingleVarianceFunc 'Rangevariance = RangeFunc

type DolanVariance = [SingleVariance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (v ': dv) = SingleVarianceKind v -> DolanVarianceKind dv

type DolanVarianceType = ListType SingleVarianceType

dolanVarianceKMCategory ::
       forall cat dv. Category cat
    => DolanVarianceType dv
    -> Dict (Category (KindMorphism (DolanVarianceKind dv) cat))
dolanVarianceKMCategory NilListType = Dict
dolanVarianceKMCategory (ConsListType _ lt) =
    case dolanVarianceKMCategory @cat lt of
        Dict -> Dict

dolanVarianceHasKM :: forall dv. DolanVarianceType dv -> Dict (HasKindMorphism (DolanVarianceKind dv))
dolanVarianceHasKM NilListType = Dict
dolanVarianceHasKM (ConsListType _ lt) =
    case dolanVarianceHasKM lt of
        Dict -> Dict

type SingleVarianceMap (v :: SingleVariance) (gt :: SingleVarianceKind v -> k)
     = forall (a :: SingleVarianceKind v) (b :: SingleVarianceKind v).
               SingleVarianceFunc v a b -> KindFunction k (gt a) (gt b)

mkRangevary ::
       forall k (f :: (Type, Type) -> k).
       (forall a b. (forall t. TypeRange t a -> TypeRange t b) -> KindFunction k (f a) (f b))
    -> SingleVarianceMap 'Rangevariance f
mkRangevary f (MkRangeFunc pbpa qaqb) = f $ \(MkTypeRange pt tq) -> MkTypeRange (pt . pbpa) (qaqb . tq)

data DolanKindVary (dv :: DolanVariance) (gt :: DolanVarianceKind dv) where
    NilDolanKindVary :: forall (gt :: Type). DolanKindVary '[] gt
    ConsDolanKindVary
        :: forall (sv :: SingleVariance) (dv :: DolanVariance) (gt :: SingleVarianceKind sv -> DolanVarianceKind dv).
           SingleVarianceMap sv gt
        -> (forall a. DolanKindVary dv (gt a))
        -> DolanKindVary (sv ': dv) gt

class HasDolanVary (dk :: DolanVariance) (f :: DolanVarianceKind dk) | f -> dk where
    dolanVary :: DolanKindVary dk f

instance HasDolanVary '[] f where
    dolanVary = NilDolanKindVary

instance HasDolanVary '[ 'Covariance] [] where
    dolanVary = ConsDolanKindVary fmap $ NilDolanKindVary

instance HasDolanVary '[ 'Covariance] ((->) a) where
    dolanVary = ConsDolanKindVary fmap $ NilDolanKindVary

instance HasDolanVary '[ 'Covariance] ((,) a) where
    dolanVary = ConsDolanKindVary fmap $ NilDolanKindVary

instance HasDolanVary '[ 'Covariance] (Either a) where
    dolanVary = ConsDolanKindVary fmap $ NilDolanKindVary

instance HasDolanVary '[ 'Contravariance, 'Covariance] (->) where
    dolanVary =
        ConsDolanKindVary (\(MkCatDual conv) -> MkNestedMorphism $ \f -> f . conv) $ dolanVary @('[ 'Covariance])

instance HasDolanVary '[ 'Covariance, 'Covariance] (,) where
    dolanVary = ConsDolanKindVary (\conv -> MkNestedMorphism $ \(a, b) -> (conv a, b)) $ dolanVary @('[ 'Covariance])

instance HasDolanVary '[ 'Covariance, 'Covariance] Either where
    dolanVary =
        ConsDolanKindVary
            (\conv ->
                 MkNestedMorphism $ \case
                     Left a -> Left $ conv a
                     Right b -> Right b) $
        dolanVary @('[ 'Covariance])

type InVarianceKind sv (a :: SingleVarianceKind sv) = InKind a
