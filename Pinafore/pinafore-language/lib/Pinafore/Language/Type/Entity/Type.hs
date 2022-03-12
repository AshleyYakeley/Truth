module Pinafore.Language.Type.Entity.Type where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.ExprShow
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Shapes

type EntityProperties :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data EntityProperties dv gt = MkEntityProperties
    { epKind :: CovaryType dv
    , epShowType :: ListTypeExprShow dv
    , epCovaryMap :: CovaryMap gt
    , epAdapter :: forall t. Arguments EntityAdapter gt t -> EntityAdapter t
    }

type SealedEntityProperties :: forall k. k -> Type
data SealedEntityProperties gt where
    MkSealedEntityProperties
        :: forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
           EntityProperties dv gt
        -> SealedEntityProperties gt

data EntityGroundType :: forall k. k -> Type where
    MkEntityGroundType :: forall k (gt :: k). FamilyType gt -> SealedEntityProperties gt -> EntityGroundType gt

sameDV ::
       forall dva dvb. (DolanVarianceKind dva ~ DolanVarianceKind dvb)
    => CovaryType dva
    -> CovaryType dvb
    -> dva :~: dvb
sameDV NilListType NilListType = Refl
sameDV (ConsListType Refl cva) (ConsListType Refl cvb) =
    case sameDV cva cvb of
        Refl -> Refl

entityToPinaforeGroundType ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       CovaryType dv
    -> EntityGroundType t
    -> PinaforeGroundType dv t
entityToPinaforeGroundType cv (MkEntityGroundType pgtFamilyType (MkSealedEntityProperties MkEntityProperties {..})) =
    case sameDV cv epKind of
        Refl -> let
            pgtVarianceType = covaryToDolanVarianceType epKind
            pgtVarianceMap = covaryToDolanVarianceMap epKind epCovaryMap
            pgtShowType = epShowType
            pgtGreatestDynamicSupertype _ = Nothing
            in MkPinaforeGroundType {..}

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
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). FamilyType t -> EntityProperties dv t -> EntityFamily
singleEntityFamily (MkFamilyType wit t) eprops =
    MkEntityFamily wit $ \t' -> do
        HRefl <- testHetEquality t t'
        return $ MkSealedEntityProperties eprops

pinaforeEntityFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       PinaforeGroundType dv t
    -> (ListTypeExprShow dv -> EntityProperties dv t)
    -> EntityFamily
pinaforeEntityFamily MkPinaforeGroundType {..} eprops = singleEntityFamily pgtFamilyType $ eprops pgtShowType

simplePinaforeEntityFamily ::
       forall (gt :: Type). Eq gt
    => PinaforeGroundType '[] gt
    -> EntityAdapter gt
    -> EntityFamily
simplePinaforeEntityFamily gt adapter =
    pinaforeEntityFamily gt $ \epShowType -> let
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
    -> (Text, Int)
showEntityType NilListType sh NilArguments = sh
showEntityType (ConsListType _ cv) sh (ConsArguments ta tr) = showEntityType cv (sh $ exprShowPrec ta) tr

instance ExprShow (MonoEntityType t) where
    exprShowPrec (MkMonoType (MkEntityGroundType _ (MkSealedEntityProperties eprops)) args) =
        showEntityType (epKind eprops) (epShowType eprops) args

instance Show (MonoEntityType t) where
    show t = unpack $ exprShow t

monoEntityAdapter :: forall t. MonoEntityType t -> EntityAdapter t
monoEntityAdapter (MkMonoType gt args) = entityGroundTypeAdapter gt args
