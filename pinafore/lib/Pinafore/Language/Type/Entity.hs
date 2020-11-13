module Pinafore.Language.Type.Entity where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicEntity
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Literal
import Pinafore.Language.Type.OpenEntity
import Pinafore.Language.Type.Show
import Shapes

type EntityGroundType :: forall k. k -> Type
data EntityGroundType t where
    TopEntityGroundType :: EntityGroundType Entity
    OpenEntityGroundType :: OpenEntityType tid -> EntityGroundType (OpenEntity tid)
    TopDynamicEntityGroundType :: EntityGroundType DynamicEntity
    ADynamicEntityGroundType :: Name -> DynamicEntityType -> EntityGroundType DynamicEntity
    LiteralEntityGroundType :: LiteralType t -> EntityGroundType t
    MaybeEntityGroundType :: EntityGroundType Maybe
    ListEntityGroundType :: EntityGroundType []
    PairEntityGroundType :: EntityGroundType (,)
    EitherEntityGroundType :: EntityGroundType Either
    ClosedEntityGroundType
        :: Name -> TypeIDType tid -> ClosedEntityType (Identified tid) -> EntityGroundType (Identified tid)

data ClosedEntityType (t :: Type) where
    NilClosedEntityType :: ClosedEntityType None
    ConsClosedEntityType
        :: Anchor -> ListType ConcreteEntityType tl -> ClosedEntityType tt -> ClosedEntityType (Either (HList tl) tt)

instance TestEquality ClosedEntityType where
    testEquality NilClosedEntityType NilClosedEntityType = Just Refl
    testEquality (ConsClosedEntityType a1 l1 t1) (ConsClosedEntityType a2 l2 t2)
        | a1 == a2 = do
            Refl <- testEquality l1 l2
            Refl <- testEquality t1 t2
            Just Refl
    testEquality _ _ = Nothing

closedEntityTypeEq :: ClosedEntityType t -> Dict (Eq t)
closedEntityTypeEq NilClosedEntityType = Dict
closedEntityTypeEq (ConsClosedEntityType _ t1 tr) =
    case (hListEq concreteEntityTypeEq t1, closedEntityTypeEq tr) of
        (Dict, Dict) -> Dict

concreteEntityTypeEq :: ConcreteEntityType t -> Dict (Eq t)
concreteEntityTypeEq (MkConcreteType TopEntityGroundType NilArguments) = Dict
concreteEntityTypeEq (MkConcreteType (OpenEntityGroundType _) NilArguments) = Dict
concreteEntityTypeEq (MkConcreteType TopDynamicEntityGroundType NilArguments) = Dict
concreteEntityTypeEq (MkConcreteType (ADynamicEntityGroundType _ _) NilArguments) = Dict
concreteEntityTypeEq (MkConcreteType (LiteralEntityGroundType t) NilArguments) =
    case literalTypeAsLiteral t of
        Dict -> Dict
concreteEntityTypeEq (MkConcreteType MaybeEntityGroundType (ConsArguments t NilArguments)) =
    case concreteEntityTypeEq t of
        Dict -> Dict
concreteEntityTypeEq (MkConcreteType ListEntityGroundType (ConsArguments t NilArguments)) =
    case concreteEntityTypeEq t of
        Dict -> Dict
concreteEntityTypeEq (MkConcreteType PairEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments))) =
    case (concreteEntityTypeEq ta, concreteEntityTypeEq tb) of
        (Dict, Dict) -> Dict
concreteEntityTypeEq (MkConcreteType EitherEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments))) =
    case (concreteEntityTypeEq ta, concreteEntityTypeEq tb) of
        (Dict, Dict) -> Dict
concreteEntityTypeEq (MkConcreteType (ClosedEntityGroundType _ _ t) NilArguments) =
    case closedEntityTypeEq t of
        Dict -> Dict

entityGroundTypeTestEquality ::
       forall (ka :: Type) (ta :: ka) (kb :: Type) (tb :: kb).
       EntityGroundType ta
    -> EntityGroundType tb
    -> Maybe (ta :~~: tb, Dict (CoercibleKind ka, InKind ta))
entityGroundTypeTestEquality TopEntityGroundType TopEntityGroundType = Just (HRefl, Dict)
entityGroundTypeTestEquality (OpenEntityGroundType t1) (OpenEntityGroundType t2) = do
    Refl <- testEquality t1 t2
    Just (HRefl, Dict)
entityGroundTypeTestEquality TopDynamicEntityGroundType TopDynamicEntityGroundType = Just (HRefl, Dict)
entityGroundTypeTestEquality (ADynamicEntityGroundType _ t1) (ADynamicEntityGroundType _ t2) =
    if t1 == t2
        then Just (HRefl, Dict)
        else Nothing
entityGroundTypeTestEquality (LiteralEntityGroundType t1) (LiteralEntityGroundType t2) = do
    Refl <- testEquality t1 t2
    Just (HRefl, Dict)
entityGroundTypeTestEquality MaybeEntityGroundType MaybeEntityGroundType = Just (HRefl, Dict)
entityGroundTypeTestEquality PairEntityGroundType PairEntityGroundType = Just (HRefl, Dict)
entityGroundTypeTestEquality ListEntityGroundType ListEntityGroundType = Just (HRefl, Dict)
entityGroundTypeTestEquality EitherEntityGroundType EitherEntityGroundType = Just (HRefl, Dict)
entityGroundTypeTestEquality (ClosedEntityGroundType _ sa ta) (ClosedEntityGroundType _ sb tb) = do
    Refl <- testEquality sa sb
    Refl <- testEquality ta tb
    Just (HRefl, Dict)
entityGroundTypeTestEquality _ _ = Nothing

instance TestHetEquality EntityGroundType where
    testHetEquality eta etb = fmap fst $ entityGroundTypeTestEquality eta etb

type ConcreteEntityType = ConcreteType EntityGroundType

instance IsCovaryGroundType EntityGroundType where
    groundTypeCovaryType ::
           forall (k :: Type) (t :: k) r.
           EntityGroundType t
        -> (forall (dv :: DolanVariance). k ~ DolanVarianceKind dv => CovaryType dv -> r)
        -> r
    groundTypeCovaryType TopEntityGroundType cont = cont NilListType
    groundTypeCovaryType (OpenEntityGroundType _) cont = cont NilListType
    groundTypeCovaryType TopDynamicEntityGroundType cont = cont NilListType
    groundTypeCovaryType (ADynamicEntityGroundType _ _) cont = cont NilListType
    groundTypeCovaryType (LiteralEntityGroundType _) cont = cont NilListType
    groundTypeCovaryType MaybeEntityGroundType cont = cont $ ConsListType Refl NilListType
    groundTypeCovaryType ListEntityGroundType cont = cont $ ConsListType Refl NilListType
    groundTypeCovaryType PairEntityGroundType cont = cont $ ConsListType Refl $ ConsListType Refl NilListType
    groundTypeCovaryType EitherEntityGroundType cont = cont $ ConsListType Refl $ ConsListType Refl NilListType
    groundTypeCovaryType (ClosedEntityGroundType _ _ _) cont = cont NilListType
    groundTypeCovaryMap :: forall k (t :: k). EntityGroundType t -> CovaryMap t
    groundTypeCovaryMap TopEntityGroundType = covarymap
    groundTypeCovaryMap (OpenEntityGroundType _) = covarymap
    groundTypeCovaryMap TopDynamicEntityGroundType = covarymap
    groundTypeCovaryMap (ADynamicEntityGroundType _ _) = covarymap
    groundTypeCovaryMap (LiteralEntityGroundType _) = covarymap
    groundTypeCovaryMap MaybeEntityGroundType = covarymap
    groundTypeCovaryMap ListEntityGroundType = covarymap
    groundTypeCovaryMap PairEntityGroundType = covarymap
    groundTypeCovaryMap EitherEntityGroundType = covarymap
    groundTypeCovaryMap (ClosedEntityGroundType _ _ _) = covarymap

entityGroundTypeShowPrec ::
       forall w f t. (forall a. w a -> (Text, Int)) -> EntityGroundType f -> Arguments w f t -> (Text, Int)
entityGroundTypeShowPrec _ TopEntityGroundType NilArguments = ("Entity", 0)
entityGroundTypeShowPrec _ (OpenEntityGroundType n) NilArguments = exprShowPrec n
entityGroundTypeShowPrec _ TopDynamicEntityGroundType NilArguments = ("DynamicEntity", 0)
entityGroundTypeShowPrec _ (ADynamicEntityGroundType n _) NilArguments = exprShowPrec n
entityGroundTypeShowPrec _ (LiteralEntityGroundType t) NilArguments = exprShowPrec t
entityGroundTypeShowPrec es MaybeEntityGroundType (ConsArguments ta NilArguments) = ("Maybe " <> precShow 0 (es ta), 2)
entityGroundTypeShowPrec es ListEntityGroundType (ConsArguments ta NilArguments) = ("[" <> fst (es ta) <> "]", 0)
entityGroundTypeShowPrec es PairEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) =
    ("(" <> fst (es ta) <> ", " <> fst (es tb) <> ")", 0)
entityGroundTypeShowPrec es EitherEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) =
    ("Either " <> precShow 0 (es ta) <> " " <> precShow 0 (es tb), 2)
entityGroundTypeShowPrec _ (ClosedEntityGroundType n _ _) NilArguments = exprShowPrec n

instance ExprShow (ConcreteEntityType t) where
    exprShowPrec (MkConcreteType gt args) = entityGroundTypeShowPrec exprShowPrec gt args

instance Show (ConcreteEntityType t) where
    show t = unpack $ exprShow t
