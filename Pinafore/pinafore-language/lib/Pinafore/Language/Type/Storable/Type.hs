module Pinafore.Language.Type.Storable.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Type
import Shapes

type Storability :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data Storability dv gt = MkStorability
    { epKind :: CovaryType dv
    , epShowType :: ListTypeExprShow dv
    , epCovaryMap :: CovaryMap gt
    , epAdapter :: forall t. Arguments StoreAdapter gt t -> StoreAdapter t
    }

saturateStoreAdapter ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) a r.
       QShimWit 'Negative a
    -> StoreAdapter a
    -> CovaryType dv
    -> CovaryMap gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> Arguments StoreAdapter gt t -> r)
    -> r
saturateStoreAdapter _ _ NilListType NilCovaryMap call = call nilDolanArgumentsShimWit NilArguments
saturateStoreAdapter tt adapter (ConsListType Refl ct) (ConsCovaryMap ccrv cvm) call =
    saturateStoreAdapter tt adapter ct cvm $ \cta ctaa ->
        call
            (consDolanArgumentsShimWit
                 (ConsDolanVarianceMap ccrv $ covaryToDolanVarianceMap ct cvm)
                 (coCCRArgument tt)
                 cta)
            (ConsArguments adapter ctaa)

storabilitySaturatedAdapter ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) a r.
       QShimWit 'Negative a
    -> StoreAdapter a
    -> Storability dv gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> StoreAdapter t -> r)
    -> r
storabilitySaturatedAdapter tt adapter MkStorability {..} call =
    saturateStoreAdapter tt adapter epKind epCovaryMap $ \args eargs -> call args (epAdapter eargs)

type SealedStorability :: forall k. k -> Type
data SealedStorability gt where
    MkSealedStorability
        :: forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv). Storability dv gt -> SealedStorability gt

data EntityGroundType :: forall k. k -> Type where
    MkEntityGroundType :: forall k (gt :: k). FamilialType gt -> SealedStorability gt -> EntityGroundType gt

sameDV ::
       forall dva dvb. (DolanVarianceKind dva ~ DolanVarianceKind dvb)
    => CovaryType dva
    -> CovaryType dvb
    -> dva :~: dvb
sameDV NilListType NilListType = Refl
sameDV (ConsListType Refl cva) (ConsListType Refl cvb) =
    case sameDV cva cvb of
        Refl -> Refl

instance TestHetEquality EntityGroundType where
    testHetEquality (MkEntityGroundType fam1 _) (MkEntityGroundType fam2 _) = testHetEquality fam1 fam2

instance IsCovaryGroundType EntityGroundType where
    groundTypeCovaryType ::
           forall (k :: Type) (t :: k) r.
           EntityGroundType t
        -> (forall (dv :: DolanVariance). k ~ DolanVarianceKind dv => CovaryType dv -> r)
        -> r
    groundTypeCovaryType (MkEntityGroundType _ (MkSealedStorability eprops)) cont = cont $ epKind eprops
    groundTypeCovaryMap :: forall k (t :: k). EntityGroundType t -> CovaryMap t
    groundTypeCovaryMap (MkEntityGroundType _ (MkSealedStorability eprops)) = epCovaryMap eprops

entityGroundTypeAdapter :: forall f t. EntityGroundType f -> Arguments MonoEntityType f t -> StoreAdapter t
entityGroundTypeAdapter (MkEntityGroundType _ (MkSealedStorability eprops)) args =
    epAdapter eprops $ mapArguments monoStoreAdapter args

data StorableFamily where
    MkStorableFamily
        :: forall (fam :: FamilyKind).
           IOWitness ('MkWitKind fam)
        -> (forall k (gt :: k). fam gt -> Maybe (SealedStorability gt))
        -> StorableFamily

singleStorableFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). FamilialType t -> Storability dv t -> StorableFamily
singleStorableFamily (MkFamilialType wit t) eprops =
    MkStorableFamily wit $ \t' -> do
        HRefl <- testHetEquality t t'
        return $ MkSealedStorability eprops

qStorableFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       QGroundType dv t
    -> (ListTypeExprShow dv -> Storability dv t)
    -> StorableFamily
qStorableFamily MkPinaforeGroundType {..} eprops = singleStorableFamily pgtFamilyType $ eprops pgtShowType

simplePinaforeStorableFamily ::
       forall (gt :: Type). Eq gt
    => QGroundType '[] gt
    -> StoreAdapter gt
    -> StorableFamily
simplePinaforeStorableFamily gt adapter =
    qStorableFamily gt $ \epShowType -> let
        epKind = NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments StoreAdapter gt t -> StoreAdapter t
        epAdapter NilArguments = adapter
        in MkStorability {..}

type MonoEntityType = MonoType EntityGroundType

showEntityType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv) a.
       CovaryType dv
    -> ListTypeExprShow dv
    -> Arguments (MonoType EntityGroundType) t a
    -> PrecNamedText
showEntityType NilListType sh NilArguments = sh
showEntityType (ConsListType _ cv) sh (ConsArguments ta tr) = showEntityType cv (sh $ exprShowPrec ta) tr

instance ExprShow (MonoEntityType t) where
    exprShowPrec (MkMonoType (MkEntityGroundType _ (MkSealedStorability eprops)) args) =
        showEntityType (epKind eprops) (epShowType eprops) args

instance Show (MonoEntityType t) where
    show t = unpack $ toText $ exprShow t

monoStoreAdapter :: forall t. MonoEntityType t -> StoreAdapter t
monoStoreAdapter (MkMonoType gt args) = entityGroundTypeAdapter gt args
