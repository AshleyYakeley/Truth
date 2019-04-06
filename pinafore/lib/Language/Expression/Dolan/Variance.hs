module Language.Expression.Dolan.Variance where

import Language.Expression.Dolan.Range
import Shapes

data CovaryMap (cat :: Type -> Type -> Type) (f :: k) where
    NilCovaryMap :: forall (cat :: Type -> Type -> Type) (f :: Type). CovaryMap cat f
    ConsCovaryMap
        :: forall (cat :: Type -> Type -> Type) (k :: Type) (f :: Type -> k).
           (forall (a :: Type) (b :: Type). cat a b -> KindMorphism cat (f a) (f b))
        -> (forall (a :: Type). CovaryMap cat (f a))
        -> CovaryMap cat f

covaryMapHasKM :: forall k (f :: k). CovaryMap (->) f -> Dict (HasKindMorphism k)
covaryMapHasKM NilCovaryMap = Dict
covaryMapHasKM (ConsCovaryMap _ cv) =
    case covaryMapHasKM cv of
        Dict -> Dict

bijectCovaryMap :: forall k (f :: k). CovaryMap (->) f -> CovaryMap Bijection f
bijectCovaryMap NilCovaryMap = NilCovaryMap
bijectCovaryMap (ConsCovaryMap f cv) =
    case covaryMapHasKM cv of
        Dict -> ConsCovaryMap (\(MkIsomorphism ab ba) -> mkKindBijection @_ @(->) (f ab) (f ba)) $ bijectCovaryMap cv

class HasCovaryMap (f :: k) where
    covarymap :: CovaryMap (->) f

instance HasCovaryMap (f :: Type) where
    covarymap = NilCovaryMap

instance HasCovaryMap Maybe where
    covarymap = ConsCovaryMap fmap NilCovaryMap

instance HasCovaryMap [] where
    covarymap = ConsCovaryMap fmap NilCovaryMap

instance HasCovaryMap (,) where
    covarymap = ConsCovaryMap (\conv -> MkNestedMorphism $ \(a, b) -> (conv a, b)) $ ConsCovaryMap fmap NilCovaryMap

instance HasCovaryMap Either where
    covarymap =
        ConsCovaryMap
            (\conv ->
                 MkNestedMorphism $ \case
                     Left a -> Left $ conv a
                     Right b -> Right b) $
        ConsCovaryMap fmap NilCovaryMap

---
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

type family SingleVarianceFunc (cat :: Type -> Type -> Type) (v :: SingleVariance) :: SingleVarianceKind v -> SingleVarianceKind v -> Type where
    SingleVarianceFunc cat 'Covariance = cat
    SingleVarianceFunc cat 'Contravariance = CatDual cat
    SingleVarianceFunc cat 'Rangevariance = WithRange cat

type DolanVariance = [SingleVariance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (v ': dv) = SingleVarianceKind v -> DolanVarianceKind dv

type DolanVarianceType = ListType SingleVarianceType

dolanVarianceKMCategory ::
       forall cat dv. Category cat
    => DolanVarianceType dv
    -> Dict (Category (KindMorphism cat :: DolanVarianceKind dv -> DolanVarianceKind dv -> Type))
dolanVarianceKMCategory NilListType = Dict
dolanVarianceKMCategory (ConsListType _ lt) =
    case dolanVarianceKMCategory @cat lt of
        Dict -> Dict

dolanVarianceHasKM :: forall dv. DolanVarianceType dv -> Dict (HasKindMorphism (DolanVarianceKind dv))
dolanVarianceHasKM NilListType = Dict
dolanVarianceHasKM (ConsListType _ lt) =
    case dolanVarianceHasKM lt of
        Dict -> Dict

type SingleVarianceMap (cat :: Type -> Type -> Type) (v :: SingleVariance) (gt :: SingleVarianceKind v -> k)
     = forall (a :: SingleVarianceKind v) (b :: SingleVarianceKind v).
               SingleVarianceFunc cat v a b -> KindMorphism cat (gt a) (gt b)

mkRangevary ::
       forall k (cat :: Type -> Type -> Type) (f :: (Type, Type) -> k). Category cat
    => (forall a b. (forall t. GenRange cat t a -> GenRange cat t b) -> KindMorphism cat (f a) (f b))
    -> SingleVarianceMap cat 'Rangevariance f
mkRangevary f (MkWithRange pbpa qaqb) = f $ \(MkRange pt tq) -> MkRange (pt . pbpa) (qaqb . tq)

data DolanVarianceMap (cat :: Type -> Type -> Type) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) where
    NilDolanVarianceMap :: forall (cat :: Type -> Type -> Type) (gt :: Type). DolanVarianceMap cat '[] gt
    ConsDolanVarianceMap
        :: forall (cat :: Type -> Type -> Type) (sv :: SingleVariance) (dv :: DolanVariance) (gt :: SingleVarianceKind sv -> DolanVarianceKind dv).
           SingleVarianceMap cat sv gt
        -> (forall a. DolanVarianceMap cat dv (gt a))
        -> DolanVarianceMap cat (sv ': dv) gt

bijectSingleVarianceMap ::
       forall (k :: Type) (sv :: SingleVariance) (f :: SingleVarianceKind sv -> k). HasKindMorphism k
    => SingleVarianceType sv
    -> SingleVarianceMap (->) sv f
    -> SingleVarianceMap Bijection sv f
bijectSingleVarianceMap CovarianceType svm (MkIsomorphism ab ba) = mkKindBijection @_ @(->) (svm ab) (svm ba)
bijectSingleVarianceMap ContravarianceType svm (MkCatDual (MkIsomorphism ab ba)) =
    mkKindBijection @_ @(->) (svm $ MkCatDual ab) (svm $ MkCatDual ba)
bijectSingleVarianceMap RangevarianceType svm (MkWithRange (MkIsomorphism pab pba) (MkIsomorphism qab qba)) =
    mkKindBijection @_ @(->) (svm $ MkWithRange pab qab) (svm $ MkWithRange pba qba)

bijectDolanVarianceMap ::
       forall dv f. DolanVarianceType dv -> DolanVarianceMap (->) dv f -> DolanVarianceMap Bijection dv f
bijectDolanVarianceMap NilListType NilDolanVarianceMap = NilDolanVarianceMap
bijectDolanVarianceMap (ConsListType svt dvt) (ConsDolanVarianceMap svm dvm) =
    case dolanVarianceHasKM dvt of
        Dict -> ConsDolanVarianceMap (bijectSingleVarianceMap svt svm) (bijectDolanVarianceMap dvt dvm)

class HasDolanVary (dv :: DolanVariance) (f :: DolanVarianceKind dv) | f -> dv where
    dolanVary :: DolanVarianceMap (->) dv f

instance HasDolanVary '[] f where
    dolanVary = NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] Maybe where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] [] where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] ((->) a) where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] ((,) a) where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] (Either a) where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Contravariance, 'Covariance] (->) where
    dolanVary =
        ConsDolanVarianceMap (\(MkCatDual conv) -> MkNestedMorphism $ \f -> f . conv) $ dolanVary @('[ 'Covariance])

instance HasDolanVary '[ 'Covariance, 'Covariance] (,) where
    dolanVary = ConsDolanVarianceMap (\conv -> MkNestedMorphism $ \(a, b) -> (conv a, b)) $ dolanVary @('[ 'Covariance])

instance HasDolanVary '[ 'Covariance, 'Covariance] Either where
    dolanVary =
        ConsDolanVarianceMap
            (\conv ->
                 MkNestedMorphism $ \case
                     Left a -> Left $ conv a
                     Right b -> Right b) $
        dolanVary @('[ 'Covariance])

type InVarianceKind sv (a :: SingleVarianceKind sv) = InKind a

type CovaryType = ListType ((:~:) 'Covariance)

type family CovaryKindDolanVariance (k :: Type) :: DolanVariance where
    CovaryKindDolanVariance Type = '[]
    CovaryKindDolanVariance (Type -> k) = 'Covariance ': (CovaryKindDolanVariance k)

covaryToDolanVarianceMap ::
       forall (cat :: Type -> Type -> Type) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       CovaryType dv
    -> CovaryMap cat f
    -> DolanVarianceMap cat dv f
covaryToDolanVarianceMap NilListType NilCovaryMap = NilDolanVarianceMap
covaryToDolanVarianceMap (ConsListType Refl ml) (ConsCovaryMap v1 vr) =
    ConsDolanVarianceMap v1 $ covaryToDolanVarianceMap ml vr

covaryKMCategory ::
       forall cat dv. Category cat
    => CovaryType dv
    -> Dict (Category (KindMorphism cat :: DolanVarianceKind dv -> DolanVarianceKind dv -> Type))
covaryKMCategory lc = dolanVarianceKMCategory @cat (mapListType (\Refl -> CovarianceType) lc)
