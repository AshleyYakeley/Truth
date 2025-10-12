{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Language.Convert.Sequence
    ( maybeGroundType
    , listGroundType
    , listStoreAdapter
    , listEntityConvert
    , list1GroundType
    , pairGroundType
    , pairStoreAdapter
    , pairEntityConvert
    , maybeEntityConvert
    )
where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Type

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

maybeGroundType :: QGroundType '[CoCCRVariance] Maybe
maybeGroundType = let
    storability :: Storability '[CoCCRVariance] Maybe
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot = pureStorabilityAdapter @Maybe $ \(ConsArguments t NilArguments) -> maybeStoreAdapter t
        in MkStorability{..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Maybe)|]) "Maybe"){qgtProperties = props}

-- Maybe
instance HasQGroundType '[CoCCRVariance] Maybe where
    qGroundType = maybeGroundType

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
    to (a : aa) = Right $ a :| aa
    in (listAdapter, list1Adapter)

listStoreAdapter :: StoreAdapter t -> StoreAdapter [t]
listStoreAdapter = fst . listStoreAdapters

list1StoreAdapter :: StoreAdapter t -> StoreAdapter (NonEmpty t)
list1StoreAdapter = snd . listStoreAdapters

listEntityConvert :: [Entity] -> Entity
listEntityConvert = storeAdapterConvert $ listStoreAdapter plainStoreAdapter

listGroundType :: QGroundType '[CoCCRVariance] []
listGroundType = let
    storability :: Storability '[CoCCRVariance] []
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot = pureStorabilityAdapter @[] $ \(ConsArguments t NilArguments) -> listStoreAdapter t
        in MkStorability{..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily [])|]) "List"){qgtProperties = props}

va :: TypeVarT (UVarT "a")
va = MkTypeVar $ MkSymbolType @"a"

-- []
instance HasQGroundType '[CoCCRVariance] [] where
    qGroundType = listGroundType

list1GroundType :: QGroundType '[CoCCRVariance] NonEmpty
list1GroundType = let
    storability :: Storability '[CoCCRVariance] NonEmpty
    storability = let
        stbKind = ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot = pureStorabilityAdapter @NonEmpty $ \(ConsArguments t NilArguments) -> list1StoreAdapter t
        in MkStorability{..}
    props = singleGroundProperty storabilityProperty storability
    in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily NonEmpty)|]) "List1.List.")
        { qgtProperties = props
        , qgtGreatestDynamicSupertype =
            MkPolyGreatestDynamicSupertype (ConsCCRArguments (CoNonpolarArgument va) NilCCRArguments) $ let
                tt =
                    MkDolanGroundedType listGroundType
                        $ ConsCCRArguments (CoCCRPolarArgument $ singleDolanType $ VarDolanSingularType va) NilCCRArguments
                in MkShimWit tt
                    $ MkPolarShim
                    $ purePolyComposeShim
                    $ functionToShim "nonEmpty" nonEmpty
                    . liftCoPolyShim iMeetL1
        }

-- NonEmpty
instance HasQGroundType '[CoCCRVariance] NonEmpty where
    qGroundType = list1GroundType

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

pairGroundType :: QGroundType '[CoCCRVariance, CoCCRVariance] (,)
pairGroundType = let
    storability :: Storability '[CoCCRVariance, CoCCRVariance] (,)
    storability = let
        stbKind = ConsListType Refl $ ConsListType Refl NilListType
        stbCovaryMap = covarymap
        stbAdapterExprKnot =
            pureStorabilityAdapter @(,) $ \(ConsArguments ta (ConsArguments tb NilArguments)) -> pairStoreAdapter ta tb
        in MkStorability{..}
    props = singleGroundProperty storabilityProperty storability
    showtype :: ListTypeExprShow '[CoCCRVariance, CoCCRVariance]
    showtype ta tb = namedTextPrec 3 $ precNamedText 2 ta <> " *: " <> precNamedText 3 tb
    in (singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (,))|]) showtype){qgtProperties = props}

-- (,)
instance HasQGroundType '[CoCCRVariance, CoCCRVariance] (,) where
    qGroundType = pairGroundType
