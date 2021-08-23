module Pinafore.Language.Type.Entity where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicEntity
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Literal
import Pinafore.Language.Type.OpenEntity
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
        :: Anchor -> ListType MonoEntityType tl -> ClosedEntityType tt -> ClosedEntityType (Either (HList tl) tt)

instance TestEquality ClosedEntityType where
    testEquality NilClosedEntityType NilClosedEntityType = Just Refl
    testEquality (ConsClosedEntityType a1 l1 t1) (ConsClosedEntityType a2 l2 t2)
        | a1 == a2 = do
            Refl <- testEquality l1 l2
            Refl <- testEquality t1 t2
            Just Refl
    testEquality _ _ = Nothing

instance Show (ClosedEntityType t) where
    show NilClosedEntityType = "nil"
    show (ConsClosedEntityType a tt NilClosedEntityType) = show tt <> " " <> show a
    show (ConsClosedEntityType a tt rest) = show tt <> " " <> show a <> " | " <> show rest

closedEntityTypeEq :: ClosedEntityType t -> Dict (Eq t)
closedEntityTypeEq NilClosedEntityType = Dict
closedEntityTypeEq (ConsClosedEntityType _ t1 tr) =
    case (hListEq monoEntityTypeEq t1, closedEntityTypeEq tr) of
        (Dict, Dict) -> Dict

monoEntityTypeEq :: MonoEntityType t -> Dict (Eq t)
monoEntityTypeEq (MkMonoType TopEntityGroundType NilArguments) = Dict
monoEntityTypeEq (MkMonoType (OpenEntityGroundType _) NilArguments) = Dict
monoEntityTypeEq (MkMonoType TopDynamicEntityGroundType NilArguments) = Dict
monoEntityTypeEq (MkMonoType (ADynamicEntityGroundType _ _) NilArguments) = Dict
monoEntityTypeEq (MkMonoType (LiteralEntityGroundType t) NilArguments) =
    case literalTypeAsLiteral t of
        Dict -> Dict
monoEntityTypeEq (MkMonoType MaybeEntityGroundType (ConsArguments t NilArguments)) =
    case monoEntityTypeEq t of
        Dict -> Dict
monoEntityTypeEq (MkMonoType ListEntityGroundType (ConsArguments t NilArguments)) =
    case monoEntityTypeEq t of
        Dict -> Dict
monoEntityTypeEq (MkMonoType PairEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments))) =
    case (monoEntityTypeEq ta, monoEntityTypeEq tb) of
        (Dict, Dict) -> Dict
monoEntityTypeEq (MkMonoType EitherEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments))) =
    case (monoEntityTypeEq ta, monoEntityTypeEq tb) of
        (Dict, Dict) -> Dict
monoEntityTypeEq (MkMonoType (ClosedEntityGroundType _ _ t) NilArguments) =
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

type MonoEntityType = MonoType EntityGroundType

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

instance ExprShow (MonoEntityType t) where
    exprShowPrec (MkMonoType gt args) = entityGroundTypeShowPrec exprShowPrec gt args

instance Show (MonoEntityType t) where
    show t = unpack $ exprShow t
