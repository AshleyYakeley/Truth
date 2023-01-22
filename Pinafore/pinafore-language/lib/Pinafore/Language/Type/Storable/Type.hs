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
    { stbKind :: CovaryType dv
    , stbCovaryMap :: CovaryMap gt
    , stbAdapter :: forall t. Arguments StoreAdapter gt t -> StoreAdapter t
    }

storabilityProperty :: IOWitness (Storability '[] ())
storabilityProperty = $(iowitness [t|Storability '[] ()|])

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
    saturateStoreAdapter tt adapter stbKind stbCovaryMap $ \args eargs -> call args (stbAdapter eargs)

type SealedStorability :: forall k. k -> Type
data SealedStorability gt where
    MkSealedStorability
        :: forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
           ListTypeExprShow dv
        -> Storability dv gt
        -> SealedStorability gt

data StorableGroundType :: forall k. k -> Type where
    MkStorableGroundType :: forall k (gt :: k). FamilialType gt -> SealedStorability gt -> StorableGroundType gt

sameDV ::
       forall dva dvb. (DolanVarianceKind dva ~ DolanVarianceKind dvb)
    => CovaryType dva
    -> CovaryType dvb
    -> dva :~: dvb
sameDV NilListType NilListType = Refl
sameDV (ConsListType Refl cva) (ConsListType Refl cvb) =
    case sameDV cva cvb of
        Refl -> Refl

instance TestHetEquality StorableGroundType where
    testHetEquality (MkStorableGroundType fam1 _) (MkStorableGroundType fam2 _) = testHetEquality fam1 fam2

instance IsCovaryGroundType StorableGroundType where
    groundTypeCovaryType ::
           forall (k :: Type) (t :: k) r.
           StorableGroundType t
        -> (forall (dv :: DolanVariance). k ~ DolanVarianceKind dv => CovaryType dv -> r)
        -> r
    groundTypeCovaryType (MkStorableGroundType _ (MkSealedStorability _ storability)) cont = cont $ stbKind storability
    groundTypeCovaryMap :: forall k (t :: k). StorableGroundType t -> CovaryMap t
    groundTypeCovaryMap (MkStorableGroundType _ (MkSealedStorability _ storability)) = stbCovaryMap storability

storableGroundTypeAdapter :: forall f t. StorableGroundType f -> Arguments MonoStorableType f t -> StoreAdapter t
storableGroundTypeAdapter (MkStorableGroundType _ (MkSealedStorability _ storability)) args =
    stbAdapter storability $ mapArguments monoStoreAdapter args

data StorableFamily where
    MkStorableFamily
        :: forall (fam :: FamilyKind).
           IOWitness ('MkWitKind fam)
        -> (forall k (gt :: k). fam gt -> Maybe (SealedStorability gt))
        -> StorableFamily

singleStorableFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       FamilialType t
    -> ListTypeExprShow dv
    -> Storability dv t
    -> StorableFamily
singleStorableFamily (MkFamilialType wit t) showType storability =
    MkStorableFamily wit $ \t' -> do
        HRefl <- testHetEquality t t'
        return $ MkSealedStorability showType storability

qStorableFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). QGroundType dv t -> Storability dv t -> StorableFamily
qStorableFamily MkQGroundType {..} storability = singleStorableFamily qgtFamilyType qgtShowType storability

simplePinaforeStorableFamily ::
       forall (gt :: Type). Eq gt
    => QGroundType '[] gt
    -> StoreAdapter gt
    -> StorableFamily
simplePinaforeStorableFamily gt adapter = let
    stbKind = NilListType
    stbCovaryMap = covarymap
    stbAdapter :: forall t. Arguments StoreAdapter gt t -> StoreAdapter t
    stbAdapter NilArguments = adapter
    in qStorableFamily gt MkStorability {..}

type MonoStorableType = MonoType StorableGroundType

showEntityType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv) a.
       CovaryType dv
    -> ListTypeExprShow dv
    -> Arguments MonoStorableType t a
    -> PrecNamedText
showEntityType NilListType sh NilArguments = sh
showEntityType (ConsListType _ cv) sh (ConsArguments ta tr) = showEntityType cv (sh $ exprShowPrec ta) tr

instance ExprShow (MonoStorableType t) where
    exprShowPrec (MkMonoType (MkStorableGroundType _ (MkSealedStorability showType storability)) args) =
        showEntityType (stbKind storability) showType args

instance Show (MonoStorableType t) where
    show t = unpack $ toText $ exprShow t

monoStoreAdapter :: forall t. MonoStorableType t -> StoreAdapter t
monoStoreAdapter (MkMonoType gt args) = storableGroundTypeAdapter gt args
