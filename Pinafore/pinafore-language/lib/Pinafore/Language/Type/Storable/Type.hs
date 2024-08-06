module Pinafore.Language.Type.Storable.Type where

import Import
import Pinafore.Language.Interpreter ()
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground

type QStoreAdapter = Compose QOpenExpression StoreAdapter

mkQStoreAdapter :: StoreAdapter --> QStoreAdapter
mkQStoreAdapter = Compose . pure

type Storability :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data Storability dv gt = MkStorability
    { stbKind :: CovaryType dv
    , stbCovaryMap :: CovaryMap gt
    , stbAdapter :: AllFor QStoreAdapter (Arguments QStoreAdapter gt)
    }

pureStorabilityAdapter ::
       forall gt.
       (forall ta. Arguments QStoreAdapter gt ta -> QStoreAdapter ta)
    -> AllFor QStoreAdapter (Arguments QStoreAdapter gt)
pureStorabilityAdapter f = MkAllFor f

storabilityProperty :: IOWitness (Storability '[] ())
storabilityProperty = $(iowitness [t|Storability '[] ()|])

saturateStoreAdapter ::
       forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) a r.
       QShimWit 'Negative a
    -> QStoreAdapter a
    -> CovaryType dv
    -> CovaryMap gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> Arguments QStoreAdapter gt t -> r)
    -> r
saturateStoreAdapter _ _ NilListType NilCovaryMap call = call nilCCRPolarArgumentsShimWit NilArguments
saturateStoreAdapter tt adapter (ConsListType Refl ct) (ConsCovaryMap ccrv cvm) call =
    saturateStoreAdapter tt adapter ct cvm $ \cta ctaa ->
        call
            (consCCRPolarArgumentsShimWit
                 (ConsCCRVariancesMap ccrv $ covaryToCCRVariancesMap ct cvm)
                 (coCCRArgument tt)
                 cta)
            (ConsArguments adapter ctaa)

storabilitySaturatedAdapter ::
       forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) a r.
       QShimWit 'Negative a
    -> QStoreAdapter a
    -> Storability dv gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> QStoreAdapter t -> Interpreter r)
    -> Interpreter r
storabilitySaturatedAdapter tt adapter MkStorability {..} call = do
    MkAllFor stba <- return stbAdapter
    saturateStoreAdapter tt adapter stbKind stbCovaryMap $ \args eargs -> call args $ stba eargs

type SealedStorability :: forall k. k -> Type
data SealedStorability gt where
    MkSealedStorability
        :: forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
           ListTypeExprShow dv
        -> Storability dv gt
        -> SealedStorability gt

data StorableGroundType :: forall k. k -> Type where
    MkStorableGroundType :: forall k (gt :: k). FamilialType gt -> SealedStorability gt -> StorableGroundType gt

sameDV ::
       forall dva dvb. (CCRVariancesKind dva ~ CCRVariancesKind dvb)
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
        -> (forall (dv :: CCRVariances). k ~ CCRVariancesKind dv => CovaryType dv -> r)
        -> r
    groundTypeCovaryType (MkStorableGroundType _ (MkSealedStorability _ storability)) cont = cont $ stbKind storability
    groundTypeCovaryMap :: forall k (t :: k). StorableGroundType t -> CovaryMap t
    groundTypeCovaryMap (MkStorableGroundType _ (MkSealedStorability _ storability)) = stbCovaryMap storability

storableGroundTypeAdapter ::
       forall f t. StorableGroundType f -> Arguments MonoStorableType f t -> Interpreter (QStoreAdapter t)
storableGroundTypeAdapter (MkStorableGroundType _ (MkSealedStorability _ storability)) args = do
    MkAllFor stba <- return $ stbAdapter storability
    args' <- mapArgumentsM monoStoreAdapter args
    return $ stba args'

type MonoStorableType = MonoType StorableGroundType

showEntityType ::
       forall (dv :: CCRVariances) (t :: CCRVariancesKind dv) a.
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
    show = exprShowShow

monoStoreAdapter :: forall t. MonoStorableType t -> Interpreter (QStoreAdapter t)
monoStoreAdapter (MkMonoType gt args) = storableGroundTypeAdapter gt args
