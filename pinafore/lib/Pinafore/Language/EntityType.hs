module Pinafore.Language.EntityType where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Literal
import Pinafore.Language.NamedEntity
import Pinafore.Language.Show
import Shapes
import Text.Read (read)
import Truth.Core

data EntityGroundType (t :: k) where
    TopEntityGroundType :: EntityGroundType Entity
    NewEntityGroundType :: EntityGroundType NewEntity
    NamedEntityGroundType :: SymbolType name -> EntityGroundType (NamedEntity name)
    LiteralEntityGroundType :: LiteralType t -> EntityGroundType t
    MaybeEntityGroundType :: EntityGroundType Maybe
    ListEntityGroundType :: EntityGroundType []
    PairEntityGroundType :: EntityGroundType (,)
    EitherEntityGroundType :: EntityGroundType Either

entityTypeEq :: EntityType t -> Dict (Eq t)
entityTypeEq (MkEntityType TopEntityGroundType NilArguments) = Dict
entityTypeEq (MkEntityType NewEntityGroundType NilArguments) = Dict
entityTypeEq (MkEntityType (NamedEntityGroundType _) NilArguments) = Dict
entityTypeEq (MkEntityType (LiteralEntityGroundType t) NilArguments) =
    case literalTypeAsLiteral t of
        Dict -> Dict
entityTypeEq (MkEntityType MaybeEntityGroundType (ConsArguments t NilArguments)) =
    case entityTypeEq t of
        Dict -> Dict
entityTypeEq (MkEntityType ListEntityGroundType (ConsArguments t NilArguments)) =
    case entityTypeEq t of
        Dict -> Dict
entityTypeEq (MkEntityType PairEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments))) =
    case (entityTypeEq ta, entityTypeEq tb) of
        (Dict, Dict) -> Dict
entityTypeEq (MkEntityType EitherEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments))) =
    case (entityTypeEq ta, entityTypeEq tb) of
        (Dict, Dict) -> Dict
entityTypeEq NoneEntityType = Dict

entityGroundTypeTestEquality :: EntityGroundType ta -> EntityGroundType tb -> Maybe (ta :~~: tb)
entityGroundTypeTestEquality TopEntityGroundType TopEntityGroundType = Just HRefl
entityGroundTypeTestEquality NewEntityGroundType NewEntityGroundType = Just HRefl
entityGroundTypeTestEquality (NamedEntityGroundType n1) (NamedEntityGroundType n2) = do
    Refl <- testEquality n1 n2
    Just HRefl
entityGroundTypeTestEquality (LiteralEntityGroundType t1) (LiteralEntityGroundType t2) = do
    Refl <- testEquality t1 t2
    Just HRefl
entityGroundTypeTestEquality MaybeEntityGroundType MaybeEntityGroundType = Just HRefl
entityGroundTypeTestEquality PairEntityGroundType PairEntityGroundType = Just HRefl
entityGroundTypeTestEquality ListEntityGroundType ListEntityGroundType = Just HRefl
entityGroundTypeTestEquality EitherEntityGroundType EitherEntityGroundType = Just HRefl
entityGroundTypeTestEquality _ _ = Nothing

data EntityType (t :: Type) where
    MkEntityType :: EntityGroundType f -> Arguments EntityType f t -> EntityType t
    NoneEntityType :: EntityType BottomType

entityGroundTypeCovaryType ::
       forall (k :: Type) (t :: k) r.
       EntityGroundType t
    -> (forall (dv :: DolanVariance). k ~ DolanVarianceKind dv => CovaryType dv -> r)
    -> r
entityGroundTypeCovaryType TopEntityGroundType cont = cont NilListType
entityGroundTypeCovaryType NewEntityGroundType cont = cont NilListType
entityGroundTypeCovaryType (NamedEntityGroundType _) cont = cont NilListType
entityGroundTypeCovaryType (LiteralEntityGroundType _) cont = cont NilListType
entityGroundTypeCovaryType MaybeEntityGroundType cont = cont $ ConsListType Refl NilListType
entityGroundTypeCovaryType ListEntityGroundType cont = cont $ ConsListType Refl NilListType
entityGroundTypeCovaryType PairEntityGroundType cont = cont $ ConsListType Refl $ ConsListType Refl NilListType
entityGroundTypeCovaryType EitherEntityGroundType cont = cont $ ConsListType Refl $ ConsListType Refl NilListType

entityGroundTypeCovaryMap :: EntityGroundType f -> CovaryMap (->) f
entityGroundTypeCovaryMap TopEntityGroundType = covarymap
entityGroundTypeCovaryMap NewEntityGroundType = covarymap
entityGroundTypeCovaryMap (NamedEntityGroundType _) = covarymap
entityGroundTypeCovaryMap (LiteralEntityGroundType _) = covarymap
entityGroundTypeCovaryMap MaybeEntityGroundType = covarymap
entityGroundTypeCovaryMap ListEntityGroundType = covarymap
entityGroundTypeCovaryMap PairEntityGroundType = covarymap
entityGroundTypeCovaryMap EitherEntityGroundType = covarymap

entityGroundTypeShowPrec ::
       forall w f t. (forall a. w a -> (Text, Int)) -> EntityGroundType f -> Arguments w f t -> (Text, Int)
entityGroundTypeShowPrec _ TopEntityGroundType NilArguments = ("Entity", 0)
entityGroundTypeShowPrec _ NewEntityGroundType NilArguments = ("NewEntity", 0)
entityGroundTypeShowPrec _ (NamedEntityGroundType n) NilArguments = (pack $ show n, 0)
entityGroundTypeShowPrec _ (LiteralEntityGroundType t) NilArguments = exprShowPrec t
entityGroundTypeShowPrec es MaybeEntityGroundType (ConsArguments ta NilArguments) = ("Maybe " <> fst (es ta), 2)
entityGroundTypeShowPrec es ListEntityGroundType (ConsArguments ta NilArguments) = ("[" <> fst (es ta) <> "]", 0)
entityGroundTypeShowPrec es PairEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) =
    ("(" <> fst (es ta) <> ", " <> fst (es tb) <> ")", 0)
entityGroundTypeShowPrec es EitherEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) =
    ("Either " <> fst (es ta) <> " " <> fst (es tb), 2)

instance ExprShow (EntityType t) where
    exprShowPrec (MkEntityType gt args) = entityGroundTypeShowPrec exprShowPrec gt args
    exprShowPrec NoneEntityType = ("None", 0)

instance Show (EntityType t) where
    show t = unpack $ exprShow t

entityGroundTypeAdapter :: forall f t. EntityGroundType f -> Arguments EntityType f t -> EntityAdapter t
entityGroundTypeAdapter TopEntityGroundType NilArguments = entityEntityAdapter
entityGroundTypeAdapter NewEntityGroundType NilArguments = isoMap MkNewEntity unNewEntity entityEntityAdapter
entityGroundTypeAdapter (NamedEntityGroundType _) NilArguments = isoMap MkNamedEntity unNamedEntity entityEntityAdapter
entityGroundTypeAdapter (LiteralEntityGroundType tl) NilArguments =
    case literalTypeAsLiteral tl of
        Dict -> let
            entityAdapterConvert = literalToEntity
            entityAdapterGet ::
                   forall m. MonadIO m
                => Entity
                -> MutableRead m PinaforeEntityRead
                -> m (Know t)
            entityAdapterGet p mr = do
                kl <- mr $ PinaforeEntityReadToLiteral p
                return $ kl >>= fromLiteral
            entityAdapterPut ::
                   forall m. MonadIO m
                => t
                -> MutableRead m PinaforeEntityRead
                -> m [PinaforeEntityEdit]
            entityAdapterPut t _mr = return [PinaforeEntityEditSetLiteral (literalToEntity t) (Known $ toLiteral t)]
            in MkEntityAdapter {..}
entityGroundTypeAdapter MaybeEntityGroundType (ConsArguments t NilArguments) = let
    justAnchor = MkAnchor $ read "c0e3fe40-598b-4c38-a28c-5be0decb1d9c"
    justAdapter = constructorEntityAdapter justAnchor $ ConsListType (entityAdapter t) NilListType
    nothingAnchor = MkAnchor $ read "0e16143d-6211-44d1-8b81-18d2700bf07f"
    nothingAdapter = constructorEntityAdapter nothingAnchor NilListType
    from :: Either (a, ()) () -> Maybe a
    from (Left (a, ())) = Just a
    from (Right ()) = Nothing
    to :: Maybe a -> Either (a, ()) ()
    to (Just a) = Left (a, ())
    to Nothing = Right ()
    in isoMap from to $ justAdapter <+++> nothingAdapter
entityGroundTypeAdapter PairEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) = let
    pairAnchor = MkAnchor $ read "d61fc4eb-8283-4be6-b3ff-a30930746ad9"
    pairAdapter =
        constructorEntityAdapter pairAnchor $
        ConsListType (entityAdapter ta) $ ConsListType (entityAdapter tb) NilListType
    from :: (a, (b, ())) -> (a, b)
    from (a, (b, ())) = (a, b)
    to :: (a, b) -> (a, (b, ()))
    to (a, b) = (a, (b, ()))
    in isoMap from to pairAdapter
entityGroundTypeAdapter EitherEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) = let
    from :: (a, ()) -> a
    from (a, ()) = a
    to :: a -> (a, ())
    to a = (a, ())
    leftAnchor = MkAnchor $ read "cceee5b7-9b2e-459c-95c1-50f62c6cc479"
    leftAdapter = isoMap from to $ constructorEntityAdapter leftAnchor $ ConsListType (entityAdapter ta) NilListType
    rightAnchor = MkAnchor $ read "dcf983ba-e126-4303-afb2-ff1d092f2e48"
    rightAdapter = isoMap from to $ constructorEntityAdapter rightAnchor $ ConsListType (entityAdapter tb) NilListType
    in leftAdapter <+++> rightAdapter
entityGroundTypeAdapter ListEntityGroundType (ConsArguments t NilArguments) = let
    nilAnchor = MkAnchor $ read "d8c742fe-7860-4961-9bfc-4be2f5a98490"
    nilAdapter = constructorEntityAdapter nilAnchor NilListType
    consAnchor = MkAnchor $ read "ff02e403-7dd1-4e34-bb2b-92824f5cb343"
    consAdapter =
        constructorEntityAdapter consAnchor $ ConsListType (entityAdapter t) $ ConsListType listAdapter NilListType
    listAdapter = isoMap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a:aa) = Right (a, (aa, ()))
    in listAdapter

entityAdapter :: forall t. EntityType t -> EntityAdapter t
entityAdapter (MkEntityType gt args) = entityGroundTypeAdapter gt args
entityAdapter NoneEntityType = isoMap never never pNone
