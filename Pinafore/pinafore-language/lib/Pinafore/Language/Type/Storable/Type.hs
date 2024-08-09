module Pinafore.Language.Type.Storable.Type where

import Import
import Pinafore.Language.Interpreter ()
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground

type Storability :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type
data Storability dv gt = MkStorability
    { stbKind :: CovaryType dv
    , stbCovaryMap :: CovaryMap gt
    , stbAdapterExpr :: QOpenExpression (AllFor StoreAdapter (Arguments StoreAdapter gt))
    }

pureStorabilityAdapter ::
       forall gt.
       (forall ta. Arguments StoreAdapter gt ta -> StoreAdapter ta)
    -> QOpenExpression (AllFor StoreAdapter (Arguments StoreAdapter gt))
pureStorabilityAdapter f = pure $ MkAllFor f

storabilityProperty :: IOWitness (Storability '[] ())
storabilityProperty = $(iowitness [t|Storability '[] ()|])

saturateStoreAdapter ::
       forall (f :: Type -> Type) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) a r. Applicative f
    => QShimWit 'Negative a
    -> f (StoreAdapter a)
    -> CovaryType dv
    -> CovaryMap gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> f (Arguments StoreAdapter gt t) -> r)
    -> r
saturateStoreAdapter _ _ NilListType NilCovaryMap call = call nilCCRPolarArgumentsShimWit $ pure NilArguments
saturateStoreAdapter tt fadapter (ConsListType Refl ct) (ConsCovaryMap ccrv cvm) call =
    saturateStoreAdapter tt fadapter ct cvm $ \cta fctargs ->
        call
            (consCCRPolarArgumentsShimWit
                 (ConsCCRVariancesMap ccrv $ covaryToCCRVariancesMap ct cvm)
                 (coCCRArgument tt)
                 cta)
            (liftA2 ConsArguments fadapter fctargs)

storabilitySaturatedAdapter ::
       forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) a r.
       QShimWit 'Negative a
    -> QOpenExpression (StoreAdapter a)
    -> Storability dv gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> QOpenExpression (StoreAdapter t) -> r)
    -> r
storabilitySaturatedAdapter tt fadapter MkStorability {..} call =
    saturateStoreAdapter tt fadapter stbKind stbCovaryMap $ \args eargsexpr ->
        call args $ liftA2 (\(MkAllFor adp) eargs -> adp eargs) stbAdapterExpr eargsexpr

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
       forall f t.
       StorableGroundType f
    -> Arguments MonoStorableType f t
    -> Interpreter (QOpenExpression (StoreAdapter t))
storableGroundTypeAdapter (MkStorableGroundType _ (MkSealedStorability _ storability)) args = do
    argsexpr' <- getCompose $ mapArgumentsM (Compose . monoStoreAdapter) args
    return $ liftA2 (\(MkAllFor stba) args' -> stba args') (stbAdapterExpr storability) argsexpr'

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

monoStoreAdapter :: forall t. MonoStorableType t -> Interpreter (QOpenExpression (StoreAdapter t))
monoStoreAdapter (MkMonoType gt args) = storableGroundTypeAdapter gt args
