module Pinafore.Language.Type.Entity.Type where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Type
import Shapes

type EntityProperties :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data EntityProperties dv gt = MkEntityProperties
    { epKind :: CovaryType dv
    , epShowType :: ListTypeExprShow dv
    , epCovaryMap :: CovaryMap gt
    , epAdapter :: forall t. Arguments EntityAdapter gt t -> EntityAdapter t
    }

saturateEntityAdapter ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) a r.
       QShimWit 'Negative a
    -> EntityAdapter a
    -> CovaryType dv
    -> CovaryMap gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> Arguments EntityAdapter gt t -> r)
    -> r
saturateEntityAdapter _ _ NilListType NilCovaryMap call = call nilDolanArgumentsShimWit NilArguments
saturateEntityAdapter tt adapter (ConsListType Refl ct) (ConsCovaryMap ccrv cvm) call =
    saturateEntityAdapter tt adapter ct cvm $ \cta ctaa ->
        call
            (consDolanArgumentsShimWit
                 (ConsDolanVarianceMap ccrv $ covaryToDolanVarianceMap ct cvm)
                 (coCCRArgument tt)
                 cta)
            (ConsArguments adapter ctaa)

entityPropertiesSaturatedAdapter ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) a r.
       QShimWit 'Negative a
    -> EntityAdapter a
    -> EntityProperties dv gt
    -> (forall t. QArgumentsShimWit dv gt 'Negative t -> EntityAdapter t -> r)
    -> r
entityPropertiesSaturatedAdapter tt adapter MkEntityProperties {..} call =
    saturateEntityAdapter tt adapter epKind epCovaryMap $ \args eargs -> call args (epAdapter eargs)

type SealedEntityProperties :: forall k. k -> Type
data SealedEntityProperties gt where
    MkSealedEntityProperties
        :: forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
           EntityProperties dv gt
        -> SealedEntityProperties gt

data EntityGroundType :: forall k. k -> Type where
    MkEntityGroundType :: forall k (gt :: k). FamilialType gt -> SealedEntityProperties gt -> EntityGroundType gt

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
    groundTypeCovaryType (MkEntityGroundType _ (MkSealedEntityProperties eprops)) cont = cont $ epKind eprops
    groundTypeCovaryMap :: forall k (t :: k). EntityGroundType t -> CovaryMap t
    groundTypeCovaryMap (MkEntityGroundType _ (MkSealedEntityProperties eprops)) = epCovaryMap eprops

entityGroundTypeAdapter :: forall f t. EntityGroundType f -> Arguments MonoEntityType f t -> EntityAdapter t
entityGroundTypeAdapter (MkEntityGroundType _ (MkSealedEntityProperties eprops)) args =
    epAdapter eprops $ mapArguments monoEntityAdapter args

data EntityFamily where
    MkEntityFamily
        :: forall (fam :: FamilyKind).
           IOWitness ('MkWitKind fam)
        -> (forall k (gt :: k). fam gt -> Maybe (SealedEntityProperties gt))
        -> EntityFamily

singleEntityFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). FamilialType t -> EntityProperties dv t -> EntityFamily
singleEntityFamily (MkFamilialType wit t) eprops =
    MkEntityFamily wit $ \t' -> do
        HRefl <- testHetEquality t t'
        return $ MkSealedEntityProperties eprops

qEntityFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       QGroundType dv t
    -> (ListTypeExprShow dv -> EntityProperties dv t)
    -> EntityFamily
qEntityFamily MkPinaforeGroundType {..} eprops = singleEntityFamily pgtFamilyType $ eprops pgtShowType

simplePinaforeEntityFamily ::
       forall (gt :: Type). Eq gt
    => QGroundType '[] gt
    -> EntityAdapter gt
    -> EntityFamily
simplePinaforeEntityFamily gt adapter =
    qEntityFamily gt $ \epShowType -> let
        epKind = NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments EntityAdapter gt t -> EntityAdapter t
        epAdapter NilArguments = adapter
        in MkEntityProperties {..}

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
    exprShowPrec (MkMonoType (MkEntityGroundType _ (MkSealedEntityProperties eprops)) args) =
        showEntityType (epKind eprops) (epShowType eprops) args

instance Show (MonoEntityType t) where
    show t = unpack $ toText $ exprShow t

monoEntityAdapter :: forall t. MonoEntityType t -> EntityAdapter t
monoEntityAdapter (MkMonoType gt args) = entityGroundTypeAdapter gt args
