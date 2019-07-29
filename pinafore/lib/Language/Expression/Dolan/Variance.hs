module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

data CovaryMap (cat :: forall kc. kc -> kc -> Type) (f :: k) where
    NilCovaryMap :: forall (cat :: forall kc. kc -> kc -> Type) (f :: Type). CovaryMap cat f
    ConsCovaryMap
        :: forall (cat :: forall kc. kc -> kc -> Type) (k :: Type) (f :: Type -> k). InKind f
        => Maybe (Dict (RepresentationalRole f))
        -> (forall (a :: Type) (b :: Type). cat a b -> cat (f a) (f b))
        -> (forall (a :: Type). CovaryMap cat (f a))
        -> CovaryMap cat f

covaryMapInKind :: forall (cat :: forall kc. kc -> kc -> Type) k (f :: k). CovaryMap cat f -> Dict (InKind f)
covaryMapInKind NilCovaryMap = Dict
covaryMapInKind (ConsCovaryMap _ _ cvm) =
    case covaryMapInKind @cat cvm of
        Dict -> Dict

bijectCovaryMap :: forall k (f :: k). CovaryMap JMShim f -> CovaryMap JMIsoShim f
bijectCovaryMap NilCovaryMap = NilCovaryMap
bijectCovaryMap (ConsCovaryMap mrr f cv) =
    ConsCovaryMap mrr (\(MkJMIsoShim (MkIsomorphism ab ba)) -> MkJMIsoShim $ MkIsomorphism (f ab) (f ba)) $
    bijectCovaryMap cv

class HasCovaryMap (f :: k) where
    covarymap :: CovaryMap JMShim f

instance HasCovaryMap (f :: Type) where
    covarymap = NilCovaryMap

instance HasCovaryMap Maybe where
    covarymap = ConsCovaryMap (Just Dict) cfmap NilCovaryMap

instance HasCovaryMap [] where
    covarymap = ConsCovaryMap (Just Dict) cfmap NilCovaryMap

instance HasCovaryMap (,) where
    covarymap = ConsCovaryMap (Just Dict) cfmap $ ConsCovaryMap (Just Dict) cfmap NilCovaryMap

instance HasCovaryMap Either where
    covarymap = ConsCovaryMap (Just Dict) cfmap $ ConsCovaryMap (Just Dict) cfmap NilCovaryMap

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

singleVarianceCoercibleKind ::
       forall (v :: SingleVariance). SingleVarianceType v -> Dict (CoercibleKind (SingleVarianceKind v))
singleVarianceCoercibleKind CovarianceType = Dict
singleVarianceCoercibleKind ContravarianceType = Dict
singleVarianceCoercibleKind RangevarianceType = Dict

type family SingleVarianceFunc (cat :: Type -> Type -> Type) (v :: SingleVariance) :: SingleVarianceKind v -> SingleVarianceKind v -> Type where
    SingleVarianceFunc cat 'Covariance = cat
    SingleVarianceFunc cat 'Contravariance = CatDual cat
    SingleVarianceFunc cat 'Rangevariance = CatRange cat

type DolanVariance = [SingleVariance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (v ': dv) = SingleVarianceKind v -> DolanVarianceKind dv

type DolanVarianceType = ListType SingleVarianceType

class LiftPolyCategory cat => DolanVarianceInCategory (cat :: forall k. k -> k -> Type) where
    dolanVarianceInCategory ::
           forall dv.
           DolanVarianceType dv
        -> Dict ( CoercibleKind (DolanVarianceKind dv)
                , InCategory (cat :: DolanVarianceKind dv -> DolanVarianceKind dv -> Type))

instance DolanVarianceInCategory JMShim where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @JMShim lt of
            Dict -> Dict

instance DolanVarianceInCategory JMIsoShim where
    dolanVarianceInCategory NilListType = Dict
    dolanVarianceInCategory (ConsListType _ lt) =
        case dolanVarianceInCategory @JMShim lt of
            Dict -> Dict

dolanVarianceHasKM :: forall dv. DolanVarianceType dv -> Dict (HasKindMorphism (DolanVarianceKind dv))
dolanVarianceHasKM NilListType = Dict
dolanVarianceHasKM (ConsListType _ lt) =
    case dolanVarianceHasKM lt of
        Dict -> Dict

type SingleVarianceMap (cat :: forall kc. kc -> kc -> Type) (v :: SingleVariance) (gt :: SingleVarianceKind v -> k)
     = forall (a :: SingleVarianceKind v) (b :: SingleVarianceKind v).
           (InKind a, InKind b) => SingleVarianceFunc cat v a b -> cat (gt a) (gt b)

mkRangevary ::
       forall k (cat :: forall kc. kc -> kc -> Type) (f :: (Type, Type) -> k). Category (cat :: Type -> Type -> Type)
    => (forall a b. (forall t. Range cat t a -> Range cat t b) -> cat (f a) (f b))
    -> SingleVarianceMap cat 'Rangevariance f
mkRangevary f (MkCatRange pbpa qaqb) = f $ \(MkRange pt tq) -> MkRange (pt . pbpa) (qaqb . tq)

data DolanVarianceMap (cat :: forall kc. kc -> kc -> Type) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) where
    NilDolanVarianceMap :: forall (cat :: forall kc. kc -> kc -> Type) (gt :: Type). DolanVarianceMap cat '[] gt
    ConsDolanVarianceMap
        :: forall (cat :: forall kc. kc -> kc -> Type) (sv :: SingleVariance) (dv :: DolanVariance) (gt :: SingleVarianceKind sv -> DolanVarianceKind dv).
           InKind gt
        => Maybe (Dict (RepresentationalRole gt))
        -> SingleVarianceMap cat sv gt
        -> (forall a. DolanVarianceMap cat dv (gt a))
        -> DolanVarianceMap cat (sv ': dv) gt

dolanVarianceMapInKind ::
       forall (cat :: forall kc. kc -> kc -> Type) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       DolanVarianceMap cat dv gt
    -> Dict (InKind gt)
dolanVarianceMapInKind NilDolanVarianceMap = Dict
dolanVarianceMapInKind (ConsDolanVarianceMap _ _ dvm) =
    case dolanVarianceMapInKind @cat dvm of
        Dict -> Dict

bijectSingleVarianceMap :: SingleVarianceType sv -> SingleVarianceMap JMShim sv gt -> SingleVarianceMap JMIsoShim sv gt
bijectSingleVarianceMap CovarianceType svm (MkJMIsoShim (MkIsomorphism ab ba)) =
    MkJMIsoShim $ MkIsomorphism (svm ab) (svm ba)
bijectSingleVarianceMap ContravarianceType svm (MkCatDual (MkJMIsoShim (MkIsomorphism ab ba))) =
    MkJMIsoShim $ MkIsomorphism (svm $ MkCatDual ab) (svm $ MkCatDual ba)
bijectSingleVarianceMap RangevarianceType svm (MkCatRange (MkJMIsoShim (MkIsomorphism pab pba)) (MkJMIsoShim (MkIsomorphism qab qba))) =
    MkJMIsoShim $ MkIsomorphism (svm $ MkCatRange pab qab) (svm $ MkCatRange pba qba)

bijectDolanVarianceMap :: DolanVarianceType dv -> DolanVarianceMap JMShim dv gt -> DolanVarianceMap JMIsoShim dv gt
bijectDolanVarianceMap NilListType NilDolanVarianceMap = NilDolanVarianceMap
bijectDolanVarianceMap (ConsListType svt dvt) (ConsDolanVarianceMap mrr svm dvm) =
    ConsDolanVarianceMap mrr (bijectSingleVarianceMap svt svm) $ bijectDolanVarianceMap dvt dvm

class HasDolanVary (dv :: DolanVariance) (f :: DolanVarianceKind dv) | f -> dv where
    dolanVary :: DolanVarianceMap JMShim dv f

instance HasDolanVary '[] f where
    dolanVary = NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] Maybe where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] [] where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] ((->) a) where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] ((,) a) where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Covariance] (Either a) where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ NilDolanVarianceMap

instance HasDolanVary '[ 'Contravariance, 'Covariance] (->) where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ dolanVary @('[ 'Covariance])

instance HasDolanVary '[ 'Covariance, 'Covariance] (,) where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ dolanVary @('[ 'Covariance])

instance HasDolanVary '[ 'Covariance, 'Covariance] Either where
    dolanVary = ConsDolanVarianceMap (Just Dict) cfmap $ dolanVary @('[ 'Covariance])

type InVarianceKind sv (a :: SingleVarianceKind sv) = InKind a

type CovaryType = ListType ((:~:) 'Covariance)

type family CovaryKindDolanVariance (k :: Type) :: DolanVariance where
    CovaryKindDolanVariance Type = '[]
    CovaryKindDolanVariance (Type -> k) = 'Covariance ': (CovaryKindDolanVariance k)

covaryToDolanVarianceMap ::
       forall (cat :: forall kc. kc -> kc -> Type) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       CovaryType dv
    -> CovaryMap cat f
    -> DolanVarianceMap cat dv f
covaryToDolanVarianceMap NilListType NilCovaryMap = NilDolanVarianceMap
covaryToDolanVarianceMap (ConsListType Refl ml) (ConsCovaryMap mrr v1 vr) =
    ConsDolanVarianceMap mrr v1 $ covaryToDolanVarianceMap ml vr

covaryKMCategory ::
       forall (cat :: forall kc. kc -> kc -> Type) dv. DolanVarianceInCategory cat
    => CovaryType dv
    -> Dict ( CoercibleKind (DolanVarianceKind dv)
            , InCategory (cat :: DolanVarianceKind dv -> DolanVarianceKind dv -> Type))
covaryKMCategory lc = dolanVarianceInCategory @cat (mapListType (\Refl -> CovarianceType) lc)
