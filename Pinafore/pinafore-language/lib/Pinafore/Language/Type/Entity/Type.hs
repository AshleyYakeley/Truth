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
    { epShowType :: ListTypeExprShow dv
    , epCovaryMap :: CovaryMap gt
    , epEq :: forall (t :: Type). Arguments (MonoType EntityGroundType) gt t -> Dict (Eq t)
    , epAdapter :: forall t. Arguments MonoEntityType gt t -> EntityAdapter t
    }

data EntityGroundType :: forall k. k -> Type where
    MkEntityGroundType
        :: forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           FamilyType t
        -> CovaryType dv
        -> EntityProperties dv t
        -> EntityGroundType t

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
entityToPinaforeGroundType cv (MkEntityGroundType pgtFamilyType covaryType MkEntityProperties {..}) =
    case sameDV cv covaryType of
        Refl -> let
            pgtVarianceType = covaryToDolanVarianceType covaryType
            pgtVarianceMap = covaryToDolanVarianceMap covaryType epCovaryMap
            pgtShowType = epShowType
            pgtGreatestDynamicSupertype _ = Nothing
            in MkPinaforeGroundType {..}

instance TestHetEquality EntityGroundType where
    testHetEquality (MkEntityGroundType fam1 _ _) (MkEntityGroundType fam2 _ _) = testHetEquality fam1 fam2

instance IsCovaryGroundType EntityGroundType where
    groundTypeCovaryType ::
           forall (k :: Type) (t :: k) r.
           EntityGroundType t
        -> (forall (dv :: DolanVariance). k ~ DolanVarianceKind dv => CovaryType dv -> r)
        -> r
    groundTypeCovaryType (MkEntityGroundType _ covaryType _) cont = cont covaryType
    groundTypeCovaryMap :: forall k (t :: k). EntityGroundType t -> CovaryMap t
    groundTypeCovaryMap (MkEntityGroundType _ _ eprops) = epCovaryMap eprops

entityGroundTypeAdapter :: forall f t. EntityGroundType f -> Arguments MonoEntityType f t -> EntityAdapter t
entityGroundTypeAdapter (MkEntityGroundType _ _ eprops) = epAdapter eprops

data EntityFamily where
    MkEntityFamily
        :: forall (fam :: FamilyKind).
           IOWitness ('MkWitKind fam)
        -> (forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
                    DolanVarianceType dv -> fam t -> Maybe (EntityProperties dv t))
        -> EntityFamily

singleEntityFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       DolanVarianceType dv
    -> FamilyType t
    -> Maybe (EntityProperties dv t)
    -> EntityFamily
singleEntityFamily dvt (MkFamilyType wit t) mep =
    MkEntityFamily wit $ \dvt' t' -> do
        Refl <- testEquality dvt dvt'
        HRefl <- testHetEquality t t'
        mep

pinaforeEntityFamily ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       PinaforeGroundType dv t
    -> (ListTypeExprShow dv -> EntityProperties dv t)
    -> EntityFamily
pinaforeEntityFamily MkPinaforeGroundType {..} eprops =
    singleEntityFamily pgtVarianceType pgtFamilyType $ return $ eprops pgtShowType

simplePinaforeEntityFamily ::
       forall (gt :: Type). Eq gt
    => PinaforeGroundType '[] gt
    -> EntityAdapter gt
    -> EntityFamily
simplePinaforeEntityFamily gt adapter =
    pinaforeEntityFamily gt $ \epShowType -> let
        epCovaryMap = covarymap
        epEq :: forall (t :: Type). Arguments (MonoType EntityGroundType) gt t -> Dict (Eq t)
        epEq NilArguments = Dict
        epAdapter :: forall t. Arguments MonoEntityType gt t -> EntityAdapter t
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
    exprShowPrec (MkMonoType (MkEntityGroundType _ covaryType eprops) args) =
        showEntityType covaryType (epShowType eprops) args

instance Show (MonoEntityType t) where
    show t = unpack $ exprShow t

monoEntityTypeEq :: MonoEntityType t -> Dict (Eq t)
monoEntityTypeEq (MkMonoType (MkEntityGroundType _ _ eprops) args) = epEq eprops args

monoEntityAdapter :: forall t. MonoEntityType t -> EntityAdapter t
monoEntityAdapter (MkMonoType gt args) = entityGroundTypeAdapter gt args
