{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Entity
    ( entityGroundType
    , getMonoEntityType
    , maybeEntityConvert
    , listEntityConvert
    , pairEntityConvert
    , eitherEntityConvert
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Entity.Closed
import Pinafore.Language.Type.Entity.Dynamic
import Pinafore.Language.Type.Entity.Open
import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Types
import Shapes

maybeEntityAdapter :: EntityAdapter t -> EntityAdapter (Maybe t)
maybeEntityAdapter et = let
    justAnchor = codeAnchor "pinafore-base:Just"
    justAdapter = constructorEntityAdapter justAnchor $ ConsListType et NilListType
    nothingAnchor = codeAnchor "pinafore-base:Nothing"
    nothingAdapter = constructorEntityAdapter nothingAnchor NilListType
    from :: Either (a, ()) () -> Maybe a
    from (Left (a, ())) = Just a
    from (Right ()) = Nothing
    to :: Maybe a -> Either (a, ()) ()
    to (Just a) = Left (a, ())
    to Nothing = Right ()
    in invmap from to $ justAdapter <+++> nothingAdapter

maybeEntityConvert :: Maybe Entity -> Entity
maybeEntityConvert = entityAdapterConvert $ maybeEntityAdapter plainEntityAdapter

maybeEntityFamily :: EntityFamily
maybeEntityFamily =
    qEntityFamily maybeGroundType $ \epShowType -> let
        epKind = ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments EntityAdapter Maybe t -> EntityAdapter t
        epAdapter (ConsArguments t NilArguments) = maybeEntityAdapter t
        in MkEntityProperties {..}

listEntityAdapter :: EntityAdapter t -> EntityAdapter [t]
listEntityAdapter et = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorEntityAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter = constructorEntityAdapter consAnchor $ ConsListType et $ ConsListType listAdapter NilListType
    listAdapter = invmap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a:aa) = Right (a, (aa, ()))
    in listAdapter

listEntityConvert :: [Entity] -> Entity
listEntityConvert = entityAdapterConvert $ listEntityAdapter plainEntityAdapter

listEntityFamily :: EntityFamily
listEntityFamily =
    qEntityFamily listGroundType $ \epShowType -> let
        epKind = ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments EntityAdapter [] t -> EntityAdapter t
        epAdapter (ConsArguments t NilArguments) = listEntityAdapter t
        in MkEntityProperties {..}

pairEntityAdapter :: EntityAdapter ta -> EntityAdapter tb -> EntityAdapter (ta, tb)
pairEntityAdapter eta etb = let
    pairAnchor = codeAnchor "pinafore-base:Pair"
    pairAdapter = constructorEntityAdapter pairAnchor $ ConsListType eta $ ConsListType etb NilListType
    from :: (a, (b, ())) -> (a, b)
    from (a, (b, ())) = (a, b)
    to :: (a, b) -> (a, (b, ()))
    to (a, b) = (a, (b, ()))
    in invmap from to pairAdapter

pairEntityConvert :: (Entity, Entity) -> Entity
pairEntityConvert = entityAdapterConvert $ pairEntityAdapter plainEntityAdapter plainEntityAdapter

pairEntityFamily :: EntityFamily
pairEntityFamily =
    qEntityFamily pairGroundType $ \epShowType -> let
        epKind = ConsListType Refl $ ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments EntityAdapter (,) t -> EntityAdapter t
        epAdapter (ConsArguments ta (ConsArguments tb NilArguments)) = pairEntityAdapter ta tb
        in MkEntityProperties {..}

eitherEntityAdapter :: EntityAdapter ta -> EntityAdapter tb -> EntityAdapter (Either ta tb)
eitherEntityAdapter eta etb = let
    from :: (a, ()) -> a
    from (a, ()) = a
    to :: a -> (a, ())
    to a = (a, ())
    leftAnchor = codeAnchor "pinafore-base:Left"
    leftAdapter = invmap from to $ constructorEntityAdapter leftAnchor $ ConsListType eta NilListType
    rightAnchor = codeAnchor "pinafore-base:Right"
    rightAdapter = invmap from to $ constructorEntityAdapter rightAnchor $ ConsListType etb NilListType
    in leftAdapter <+++> rightAdapter

eitherEntityConvert :: Either Entity Entity -> Entity
eitherEntityConvert = entityAdapterConvert $ eitherEntityAdapter plainEntityAdapter plainEntityAdapter

eitherEntityFamily :: EntityFamily
eitherEntityFamily =
    qEntityFamily eitherGroundType $ \epShowType -> let
        epKind = ConsListType Refl $ ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments EntityAdapter Either t -> EntityAdapter t
        epAdapter (ConsArguments ta (ConsArguments tb NilArguments)) = eitherEntityAdapter ta tb
        in MkEntityProperties {..}

entityGroundType :: QGroundType '[] Entity
entityGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Entity)|]) "Entity"

entityEntityFamily :: EntityFamily
entityEntityFamily = simplePinaforeEntityFamily entityGroundType plainEntityAdapter

literalEntityFamily :: EntityFamily
literalEntityFamily =
    qEntityFamily literalGroundType $ \showtype ->
        MkEntityProperties
            { epKind = NilListType
            , epCovaryMap = covarymap
            , epAdapter = \NilArguments -> literalEntityAdapter id
            , epShowType = showtype
            }

literalEntityGroundType :: Codec Literal t -> QGroundType '[] t -> EntityGroundType t
literalEntityGroundType codec MkPinaforeGroundType {..} =
    MkEntityGroundType pgtFamilyType $
    MkSealedEntityProperties $
    MkEntityProperties
        { epKind = NilListType
        , epCovaryMap = covarymap
        , epAdapter = \NilArguments -> literalEntityAdapter codec
        , epShowType = pgtShowType
        }

findmap :: [a] -> (a -> Maybe b) -> Maybe b
findmap [] _ = Nothing
findmap (x:xs) f =
    case f x of
        Nothing -> findmap xs f
        Just y -> Just y

allEntityFamilies :: [EntityFamily]
allEntityFamilies =
    [ maybeEntityFamily
    , listEntityFamily
    , pairEntityFamily
    , eitherEntityFamily
    , entityEntityFamily
    , dynamicEntityFamily
    , aDynamicEntityEntityFamily
    , closedEntityFamily
    , openEntityFamily
    , literalEntityFamily
    ]

instance CovarySubtype QGroundType EntityGroundType where
    dolanToMonoGroundType ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           QGroundType dv t
        -> Maybe (CovaryType dv, EntityGroundType t)
    dolanToMonoGroundType agt@MkPinaforeGroundType {..} =
        (findmap allEntityFamilies $ \(MkEntityFamily wit ff) -> do
             tt <- matchFamilyType wit pgtFamilyType
             seprops <- ff tt
             covaryType <- dolanVarianceToCovaryType pgtVarianceType
             return (covaryType, MkEntityGroundType pgtFamilyType seprops)) <|> do
            Refl <- testEquality NilListType pgtVarianceType
            case pgtGreatestDynamicSupertype of
                SimplePolyGreatestDynamicSupertype gt from to -> do
                    (Refl, HRefl) <- groundTypeTestEquality gt literalGroundType
                    return
                        (NilListType, literalEntityGroundType (MkCodec (shimToFunction from) (shimToFunction to)) agt)
                _ -> Nothing

getMonoEntityType :: MonadThrow ErrorType m => QNonpolarType t -> m (MonoEntityType t)
getMonoEntityType tm =
    case nonpolarToMonoType tm of
        Just t -> return t
        Nothing -> throw $ InterpretTypeNotEntityError $ exprShow tm
