module Language.Expression.Dolan.Variance where

import Data.Shim
import Shapes

type DolanVariance = [Variance]

-- How many layers of type abstraction are you on?
type family DolanVarianceKind (dv :: DolanVariance) :: Type where
    DolanVarianceKind '[] = Type
    DolanVarianceKind (v ': dv) = VarianceKind v -> DolanVarianceKind dv

type DolanVarianceType = ListType VarianceType

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

data DolanVarianceMap (cat :: forall kc. kc -> kc -> Type) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) where
    NilDolanVarianceMap :: forall (cat :: forall kc. kc -> kc -> Type) (gt :: Type). DolanVarianceMap cat '[] gt
    ConsDolanVarianceMap
        :: forall (cat :: forall kc. kc -> kc -> Type) (sv :: Variance) (dv :: DolanVariance) (gt :: VarianceKind sv -> DolanVarianceKind dv).
           InKind gt
        => Maybe (Dict (RepresentationalRole gt))
        -> VarianceMap cat sv gt
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

bijectSingleVarianceMap :: VarianceType sv -> VarianceMap JMShim sv gt -> VarianceMap JMIsoShim sv gt
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
