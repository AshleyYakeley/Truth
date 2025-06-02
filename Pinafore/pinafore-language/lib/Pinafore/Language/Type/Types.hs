module Pinafore.Language.Type.Types
    ( actionGroundType
    , eitherGroundType
    , resultGroundType
    , entityMapGroundType
    , mapEntityConvert
    , showableGroundType
    , eitherEntityConvert
    , resultEntityConvert
    )
where

import Import
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Sequence
import Pinafore.Language.Type.Storable
import Pinafore.Language.Value

actionGroundType :: QGroundType '[CoCCRVariance] Action
actionGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Action)|]) "Action"

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

eitherGroundType :: QGroundType '[CoCCRVariance, CoCCRVariance] Either
eitherGroundType = let
    storability :: Storability '[CoCCRVariance, CoCCRVariance] Either
    storability = let
        stbKind = ConsListType Refl $ ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot =
            pureStorabilityAdapter @Either $ \(ConsArguments ta (ConsArguments tb NilArguments)) ->
                eitherStoreAdapter ta tb
        in MkStorability{..}
    props = singleGroundProperty storabilityProperty storability
    showtype :: ListTypeExprShow '[CoCCRVariance, CoCCRVariance]
    showtype ta tb = namedTextPrec 4 $ precNamedText 3 ta <> " +: " <> precNamedText 4 tb
    in (singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Either)|]) showtype){qgtProperties = props}

resultStoreAdapter :: StoreAdapter ta -> StoreAdapter tb -> StoreAdapter (Result ta tb)
resultStoreAdapter eta etb = let
    from :: (a, ()) -> a
    from (a, ()) = a
    to :: a -> (a, ())
    to a = (a, ())
    leftAnchor = codeAnchor "pinafore-base:Failure"
    leftAdapter = invmap from to $ constructorStoreAdapter leftAnchor $ ConsListType eta NilListType
    rightAnchor = codeAnchor "pinafore-base:Success"
    rightAdapter = invmap from to $ constructorStoreAdapter rightAnchor $ ConsListType etb NilListType
    in invmap eitherToResult resultToEither $ leftAdapter <+++> rightAdapter

resultEntityConvert :: Result Entity Entity -> Entity
resultEntityConvert = storeAdapterConvert $ resultStoreAdapter plainStoreAdapter plainStoreAdapter

resultGroundType :: QGroundType '[CoCCRVariance, CoCCRVariance] Result
resultGroundType = let
    storability :: Storability '[CoCCRVariance, CoCCRVariance] Result
    storability = let
        stbKind = ConsListType Refl $ ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot =
            pureStorabilityAdapter @Result $ \(ConsArguments ta (ConsArguments tb NilArguments)) ->
                resultStoreAdapter ta tb
        in MkStorability{..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Result)|]) "Result"){qgtProperties = props}

mapStoreAdapter :: StoreAdapter t -> StoreAdapter (EntityMap t)
mapStoreAdapter t = invmap entityMapFromList entityMapToList $ listStoreAdapter $ pairStoreAdapter plainStoreAdapter t

entityMapGroundType :: QGroundType '[CoCCRVariance] EntityMap
entityMapGroundType = let
    storability :: Storability '[CoCCRVariance] EntityMap
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot = pureStorabilityAdapter @EntityMap $ \(ConsArguments t NilArguments) -> mapStoreAdapter t
        in MkStorability{..}
    props :: GroundProperties '[CoCCRVariance] EntityMap
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily EntityMap)|]) "EntityMap")
        { qgtProperties = props
        }

mapEntityConvert :: EntityMap Entity -> Entity
mapEntityConvert = storeAdapterConvert $ mapStoreAdapter plainStoreAdapter

showableGroundType :: QGroundType '[] Showable
showableGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Showable)|]) "Showable"
