{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Storable
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
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Storable.Data
import Pinafore.Language.Type.Storable.Dynamic
import Pinafore.Language.Type.Storable.Open
import Pinafore.Language.Type.Storable.Type
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Types
import Shapes

maybeStoreAdapter :: StoreAdapter t -> StoreAdapter (Maybe t)
maybeStoreAdapter et = let
    justAnchor = codeAnchor "pinafore-base:Just"
    justAdapter = constructorStoreAdapter justAnchor $ ConsListType et NilListType
    nothingAnchor = codeAnchor "pinafore-base:Nothing"
    nothingAdapter = constructorStoreAdapter nothingAnchor NilListType
    from :: Either (a, ()) () -> Maybe a
    from (Left (a, ())) = Just a
    from (Right ()) = Nothing
    to :: Maybe a -> Either (a, ()) ()
    to (Just a) = Left (a, ())
    to Nothing = Right ()
    in invmap from to $ justAdapter <+++> nothingAdapter

maybeEntityConvert :: Maybe Entity -> Entity
maybeEntityConvert = storeAdapterConvert $ maybeStoreAdapter plainStoreAdapter

maybeStorableFamily :: StorableFamily
maybeStorableFamily =
    qStorableFamily maybeGroundType $ \epShowType -> let
        epKind = ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments StoreAdapter Maybe t -> StoreAdapter t
        epAdapter (ConsArguments t NilArguments) = maybeStoreAdapter t
        in MkStorability {..}

listStoreAdapter :: StoreAdapter t -> StoreAdapter [t]
listStoreAdapter et = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorStoreAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter = constructorStoreAdapter consAnchor $ ConsListType et $ ConsListType listAdapter NilListType
    listAdapter = invmap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a:aa) = Right (a, (aa, ()))
    in listAdapter

listEntityConvert :: [Entity] -> Entity
listEntityConvert = storeAdapterConvert $ listStoreAdapter plainStoreAdapter

listStorableFamily :: StorableFamily
listStorableFamily =
    qStorableFamily listGroundType $ \epShowType -> let
        epKind = ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments StoreAdapter [] t -> StoreAdapter t
        epAdapter (ConsArguments t NilArguments) = listStoreAdapter t
        in MkStorability {..}

pairStoreAdapter :: StoreAdapter ta -> StoreAdapter tb -> StoreAdapter (ta, tb)
pairStoreAdapter eta etb = let
    pairAnchor = codeAnchor "pinafore-base:Pair"
    pairAdapter = constructorStoreAdapter pairAnchor $ ConsListType eta $ ConsListType etb NilListType
    from :: (a, (b, ())) -> (a, b)
    from (a, (b, ())) = (a, b)
    to :: (a, b) -> (a, (b, ()))
    to (a, b) = (a, (b, ()))
    in invmap from to pairAdapter

pairEntityConvert :: (Entity, Entity) -> Entity
pairEntityConvert = storeAdapterConvert $ pairStoreAdapter plainStoreAdapter plainStoreAdapter

pairStorableFamily :: StorableFamily
pairStorableFamily =
    qStorableFamily pairGroundType $ \epShowType -> let
        epKind = ConsListType Refl $ ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments StoreAdapter (,) t -> StoreAdapter t
        epAdapter (ConsArguments ta (ConsArguments tb NilArguments)) = pairStoreAdapter ta tb
        in MkStorability {..}

eitherStoreAdapter :: StoreAdapter ta -> StoreAdapter tb -> StoreAdapter (Either ta tb)
eitherStoreAdapter eta etb = let
    from :: (a, ()) -> a
    from (a, ()) = a
    to :: a -> (a, ())
    to a = (a, ())
    leftAnchor = codeAnchor "pinafore-base:Left"
    leftAdapter = invmap from to $ constructorStoreAdapter leftAnchor $ ConsListType eta NilListType
    rightAnchor = codeAnchor "pinafore-base:Right"
    rightAdapter = invmap from to $ constructorStoreAdapter rightAnchor $ ConsListType etb NilListType
    in leftAdapter <+++> rightAdapter

eitherEntityConvert :: Either Entity Entity -> Entity
eitherEntityConvert = storeAdapterConvert $ eitherStoreAdapter plainStoreAdapter plainStoreAdapter

eitherStorableFamily :: StorableFamily
eitherStorableFamily =
    qStorableFamily eitherGroundType $ \epShowType -> let
        epKind = ConsListType Refl $ ConsListType Refl NilListType
        epCovaryMap = covarymap
        epAdapter :: forall t. Arguments StoreAdapter Either t -> StoreAdapter t
        epAdapter (ConsArguments ta (ConsArguments tb NilArguments)) = eitherStoreAdapter ta tb
        in MkStorability {..}

entityGroundType :: QGroundType '[] Entity
entityGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Entity)|]) "Entity"

entityStorableFamily :: StorableFamily
entityStorableFamily = simplePinaforeStorableFamily entityGroundType plainStoreAdapter

literalStorableFamily :: StorableFamily
literalStorableFamily =
    qStorableFamily literalGroundType $ \showtype ->
        MkStorability
            { epKind = NilListType
            , epCovaryMap = covarymap
            , epAdapter = \NilArguments -> literalStoreAdapter id
            , epShowType = showtype
            }

literalEntityGroundType :: Codec Literal t -> QGroundType '[] t -> EntityGroundType t
literalEntityGroundType codec MkPinaforeGroundType {..} =
    MkEntityGroundType pgtFamilyType $
    MkSealedStorability $
    MkStorability
        { epKind = NilListType
        , epCovaryMap = covarymap
        , epAdapter = \NilArguments -> literalStoreAdapter codec
        , epShowType = pgtShowType
        }

findmap :: [a] -> (a -> Maybe b) -> Maybe b
findmap [] _ = Nothing
findmap (x:xs) f =
    case f x of
        Nothing -> findmap xs f
        Just y -> Just y

allStorableFamilies :: [StorableFamily]
allStorableFamilies =
    [ maybeStorableFamily
    , listStorableFamily
    , pairStorableFamily
    , eitherStorableFamily
    , entityStorableFamily
    , dynamicStorableFamily
    , aDynamicEntityStorableFamily
    , dataStorableFamily
    , openStorableFamily
    , literalStorableFamily
    ]

instance CovarySubtype QGroundType EntityGroundType where
    dolanToMonoGroundType ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           QGroundType dv t
        -> Maybe (CovaryType dv, EntityGroundType t)
    dolanToMonoGroundType agt@MkPinaforeGroundType {..} =
        (findmap allStorableFamilies $ \(MkStorableFamily wit ff) -> do
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

getMonoEntityType :: QNonpolarType t -> QInterpreter (MonoEntityType t)
getMonoEntityType tm =
    case nonpolarToMonoType tm of
        Just t -> return t
        Nothing -> throwWithName $ \ntt -> InterpretTypeNotEntityError $ ntt $ exprShow tm
