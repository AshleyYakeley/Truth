module Pinafore.Language.Type.Types
    ( actionGroundType
    , maybeGroundType
    , listGroundType
    , list1GroundType
    , eitherGroundType
    , resultGroundType
    , pairGroundType
    , mapGroundType
    , mapEntityConvert
    , showableGroundType
    , maybeEntityConvert
    , listEntityConvert
    , pairEntityConvert
    , eitherEntityConvert
    , resultEntityConvert
    ) where

import Import
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Storable
import Pinafore.Language.Value

actionGroundType :: QGroundType '[ CoCCRVariance] Action
actionGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Action)|]) "Action"

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

maybeGroundType :: QGroundType '[ CoCCRVariance] Maybe
maybeGroundType = let
    storability :: Storability '[ CoCCRVariance] Maybe
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapter = pureStorabilityAdapter @Maybe $ \(ConsArguments t NilArguments) -> maybeStoreAdapter t
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Maybe)|]) "Maybe") {qgtProperties = props}

listStoreAdapters :: StoreAdapter t -> (StoreAdapter [t], StoreAdapter (NonEmpty t))
listStoreAdapters et = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorStoreAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter = constructorStoreAdapter consAnchor $ ConsListType et $ ConsListType listAdapter NilListType
    list1Adapter = invmap from1 to1 consAdapter
    listAdapter = invmap from to $ nilAdapter <+++> list1Adapter
    from1 :: (a, ([a], ())) -> NonEmpty a
    from1 (a, (aa, ())) = a :| aa
    to1 :: NonEmpty t -> (t, ([t], ()))
    to1 (a :| aa) = (a, (aa, ()))
    from :: Either () (NonEmpty a) -> [a]
    from (Left ()) = []
    from (Right (a :| aa)) = a : aa
    to :: [a] -> Either () (NonEmpty a)
    to [] = Left ()
    to (a:aa) = Right $ a :| aa
    in (listAdapter, list1Adapter)

listStoreAdapter :: StoreAdapter t -> StoreAdapter [t]
listStoreAdapter = fst . listStoreAdapters

list1StoreAdapter :: StoreAdapter t -> StoreAdapter (NonEmpty t)
list1StoreAdapter = snd . listStoreAdapters

listEntityConvert :: [Entity] -> Entity
listEntityConvert = storeAdapterConvert $ listStoreAdapter plainStoreAdapter

listGroundType :: QGroundType '[ CoCCRVariance] []
listGroundType = let
    storability :: Storability '[ CoCCRVariance] []
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapter = pureStorabilityAdapter @[] $ \(ConsArguments t NilArguments) -> listStoreAdapter t
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily [])|]) "List") {qgtProperties = props}

list1GroundType :: IsDolanSubtypeGroundType QGroundType => QGroundType '[ CoCCRVariance] NonEmpty
list1GroundType = let
    storability :: Storability '[ CoCCRVariance] NonEmpty
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapter = pureStorabilityAdapter @NonEmpty $ \(ConsArguments t NilArguments) -> list1StoreAdapter t
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily NonEmpty)|]) "List1.List.")
           { qgtProperties = props
           , qgtGreatestDynamicSupertype =
                 MkPolyGreatestDynamicSupertype $ \(ConsCCRArguments ta NilCCRArguments) -> let
                     tt = MkDolanGroundedType listGroundType $ ConsCCRArguments ta NilCCRArguments
                     in return $ Just $ MkShimWit tt (MkPolarShim $ functionToShim "nonEmpty" nonEmpty)
           }

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

eitherGroundType :: QGroundType '[ CoCCRVariance, CoCCRVariance] Either
eitherGroundType = let
    storability :: Storability '[ CoCCRVariance, CoCCRVariance] Either
    storability = let
        stbKind = ConsListType Refl $ ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapter =
            pureStorabilityAdapter @Either $ \(ConsArguments ta (ConsArguments tb NilArguments)) ->
                eitherStoreAdapter ta tb
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    showtype :: ListTypeExprShow '[ CoCCRVariance, CoCCRVariance]
    showtype ta tb = namedTextPrec 4 $ precNamedText 3 ta <> " +: " <> precNamedText 4 tb
    in (singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Either)|]) showtype) {qgtProperties = props}

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

resultGroundType :: QGroundType '[ CoCCRVariance, CoCCRVariance] Result
resultGroundType = let
    storability :: Storability '[ CoCCRVariance, CoCCRVariance] Result
    storability = let
        stbKind = ConsListType Refl $ ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapter =
            pureStorabilityAdapter @Result $ \(ConsArguments ta (ConsArguments tb NilArguments)) ->
                resultStoreAdapter ta tb
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Result)|]) "Result") {qgtProperties = props}

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

pairGroundType :: QGroundType '[ CoCCRVariance, CoCCRVariance] (,)
pairGroundType = let
    storability :: Storability '[ CoCCRVariance, CoCCRVariance] (,)
    storability = let
        stbKind = ConsListType Refl $ ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapter =
            pureStorabilityAdapter @(,) $ \(ConsArguments ta (ConsArguments tb NilArguments)) -> pairStoreAdapter ta tb
        in MkStorability {..}
    props = singleGroundProperty storabilityProperty storability
    showtype :: ListTypeExprShow '[ CoCCRVariance, CoCCRVariance]
    showtype ta tb = namedTextPrec 3 $ precNamedText 2 ta <> " *: " <> precNamedText 3 tb
    in (singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (,))|]) showtype) {qgtProperties = props}

mapStoreAdapter :: StoreAdapter t -> StoreAdapter (LangMap t)
mapStoreAdapter t = invmap langMapFromList langMapToList $ listStoreAdapter $ pairStoreAdapter plainStoreAdapter t

mapGroundType :: QGroundType '[ CoCCRVariance] LangMap
mapGroundType = let
    storability :: Storability '[ CoCCRVariance] LangMap
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapter = pureStorabilityAdapter @LangMap $ \(ConsArguments t NilArguments) -> mapStoreAdapter t
        in MkStorability {..}
    props :: GroundProperties '[ CoCCRVariance] LangMap
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMap)|]) "Map") {qgtProperties = props}

mapEntityConvert :: LangMap Entity -> Entity
mapEntityConvert = storeAdapterConvert $ mapStoreAdapter plainStoreAdapter

showableGroundType :: QGroundType '[] Showable
showableGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Showable)|]) "Showable"
