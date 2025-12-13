{-# OPTIONS -fno-warn-orphans #-}
module Pinafore.Language.Convert.Sequence
    ( Link (..)
    , LangMaybe (..)
    , maybeToLang
    , langToMaybe
    , LangList (..)
    , listLangIso
    , LangList1 (..)
    , linkEntityConvert
    , unitStoreAdapter
    , pairStoreAdapter
    , langListStoreAdapter
    )
where

import Import
import Pinafore.Language.Convert.FromQIsoShim
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Convert.JoinMeet ()
import Pinafore.Language.Convert.Recursive
import Pinafore.Language.Convert.Var
import Pinafore.Language.Type

unitStoreAdapter :: StoreAdapter ()
unitStoreAdapter = constructorStoreAdapter (codeAnchor "pinafore-base:Nil") NilListType

unitTypeCodec :: Codec (Link a b) ()
unitTypeCodec =
    MkCodec
        ( \case
            NilLink -> Just ()
            ConsLink _ _ -> Nothing
        )
        (\() -> NilLink)

pairTypeCodec :: Codec (Link a b) (a, b)
pairTypeCodec =
    MkCodec
        ( \case
            NilLink -> Nothing
            ConsLink a b -> Just (a, b)
        )
        (\(a, b) -> ConsLink a b)

instance HasQGroundType '[] () where
    qGroundType = let
        storability :: Storability '[] ()
        storability = let
            stbKind = NilListType
            stbCovaryMap = covarymap
            stbAdapterExprKnot =
                pureStorabilityAdapter @() $ \NilArguments -> unitStoreAdapter
            in MkStorability{..}
        props = singleGroundProperty storabilityProperty storability
        gds :: QPolyGreatestDynamicSupertype '[] ()
        gds =
            MkPolyGreatestDynamicSupertype
                NilCCRArguments
                $ mapNegShimWit (functionToShim "fromNilLink" $ decode unitTypeCodec) (qGroundedType :: _ (Link TopType TopType))
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ())|]) "Unit")
            { qgtProperties = props
            , qgtGreatestDynamicSupertype = gds
            }

---

pairStoreAdapter :: StoreAdapter ta -> StoreAdapter tb -> StoreAdapter (ta, tb)
pairStoreAdapter sa sb =
    invmap (\(a, (b, ())) -> (a, b)) (\(a, b) -> (a, (b, ())))
        $ constructorStoreAdapter (codeAnchor "pinafore-base:Cons")
        $ ConsListType sa
        $ ConsListType sb NilListType

-- (,)
instance HasQGroundType '[CoCCRVariance, CoCCRVariance] (,) where
    qGroundType = let
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
        gds :: QPolyGreatestDynamicSupertype '[CoCCRVariance, CoCCRVariance] (,)
        gds =
            MkPolyGreatestDynamicSupertype
                (ConsCCRArguments (CoNonpolarArgument $ mkTypeVarT @"a") $ ConsCCRArguments (CoNonpolarArgument $ mkTypeVarT @"b") NilCCRArguments)
                $ mapNegShimWit (functionToShim "fromConsLink" $ decode pairTypeCodec . coerce) (qGroundedType :: _ (Link A B))
        in (singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily (,))|]) showtype)
            { qgtProperties = props
            , qgtGreatestDynamicSupertype = gds
            }

---

data Link a b = NilLink | ConsLink a b

instance Functor (Link a) where
    fmap f = \case
        NilLink -> NilLink
        ConsLink a b -> ConsLink a $ f b

instance CatFunctor (->) (NestedMorphism (->)) Link where
    cfmap f =
        MkNestedMorphism $ \case
            NilLink -> NilLink
            ConsLink a b -> ConsLink (f a) b

instance RepresentationalRole Link where
    representationalCoercion MkCoercion = MkCoercion

instance RepresentationalRole (Link a) where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational Link where
    maybeRepresentational = Just Dict

instance MaybeRepresentational (Link a) where
    maybeRepresentational = Just Dict

instance HasVariance Link where
    type VarianceOf Link = 'Covariance

instance HasVariance (Link a) where
    type VarianceOf (Link a) = 'Covariance

linkStoreAdapter :: StoreAdapter a -> StoreAdapter b -> StoreAdapter (Link a b)
linkStoreAdapter sa sb = let
    from :: Either (a, b) () -> Link a b
    from = \case
        Left (a, b) -> ConsLink a b
        Right () -> NilLink
    to :: Link a b -> Either (a, b) ()
    to = \case
        ConsLink a b -> Left (a, b)
        NilLink -> Right ()
    in invmap from to $ pairStoreAdapter sa sb <+++> unitStoreAdapter

linkEntityConvert :: Link Entity Entity -> Entity
linkEntityConvert = storeAdapterConvert $ linkStoreAdapter plainStoreAdapter plainStoreAdapter

instance HasQGroundType '[CoCCRVariance, CoCCRVariance] Link where
    qGroundType = let
        storability :: Storability '[CoCCRVariance, CoCCRVariance] Link
        storability = let
            stbKind = ConsListType Refl $ ConsListType Refl NilListType
            stbCovaryMap = covarymap
            stbAdapterExprKnot = pureStorabilityAdapter @Link $ \(ConsArguments sa (ConsArguments sb NilArguments)) ->
                linkStoreAdapter sa sb
            in MkStorability{..}
        showtype :: ListTypeExprShow '[CoCCRVariance, CoCCRVariance]
        showtype ta tb = namedTextPrec 3 $ precNamedText 2 ta <> " *? " <> precNamedText 3 tb
        props = singleGroundProperty storabilityProperty storability
        in (singleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Link)|]) showtype){qgtProperties = props}

---

newtype LangMaybe a = MkLangMaybe (Link a ())

instance Functor LangMaybe where
    fmap ab (MkLangMaybe lau) = MkLangMaybe $ unNestedMorphism (cfmap ab) lau

instance RepresentationalRole LangMaybe where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational LangMaybe where
    maybeRepresentational = Just Dict

instance HasVariance LangMaybe where
    type VarianceOf LangMaybe = 'Covariance

maybeStoreAdapter :: forall a. StoreAdapter a -> StoreAdapter (LangMaybe a)
maybeStoreAdapter sa = coerce $ linkStoreAdapter sa unitStoreAdapter

decodeToMaybe :: Link a (Link TopType TopType) -> Maybe (LangMaybe a)
decodeToMaybe = \case
    NilLink -> Just $ MkLangMaybe NilLink
    ConsLink a NilLink -> Just $ MkLangMaybe $ ConsLink a ()
    ConsLink _ (ConsLink _ _) -> Nothing

instance HasQGroundType '[CoCCRVariance] LangMaybe where
    qGroundType = let
        storability :: Storability '[CoCCRVariance] LangMaybe
        storability = let
            stbKind = ConsListType Refl NilListType
            stbCovaryMap = covarymap
            stbAdapterExprKnot = pureStorabilityAdapter @LangMaybe $ \(ConsArguments t NilArguments) -> maybeStoreAdapter t
            in MkStorability{..}
        props = singleGroundProperty storabilityProperty storability
        gds :: QPolyGreatestDynamicSupertype '[CoCCRVariance] LangMaybe
        gds =
            MkPolyGreatestDynamicSupertype
                (ConsCCRArguments (CoNonpolarArgument $ mkTypeVarT @"a") NilCCRArguments)
                $ mapNegShimWit (functionToShim "decodeToMaybe" $ decodeToMaybe . coerce) (qGroundedType :: _ (Link A (Link TopType TopType)))
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMaybe)|]) "Maybe")
            { qgtProperties = props
            , qgtGreatestDynamicSupertype = gds
            }

maybeToLang :: Maybe a -> LangMaybe a
maybeToLang = \case
    Just x -> MkLangMaybe $ ConsLink x ()
    Nothing -> MkLangMaybe NilLink

langToMaybe :: LangMaybe a -> Maybe a
langToMaybe (MkLangMaybe link) = case link of
    ConsLink x () -> Just x
    NilLink -> Nothing

-- Maybe
instance
    forall (pshim :: PolyShimKind) polarity a.
    ( FromQIsoShim pshim
    , Is PolarityType polarity
    , HasQType pshim polarity a
    ) =>
    HasQType pshim polarity (Maybe a)
    where
    qType = mapQIsoShimWit (functionToShim "maybeToLang" maybeToLang) (functionToShim "langToMaybe" langToMaybe) qType

---

newtype LangList a = MkLangList {unLangList :: Rec0 (Link a)}

instance Functor LangList where
    fmap ab (MkLangList rla) = MkLangList $ mapRec0 (cfmap1 ab) rla

instance RepresentationalRole LangList where
    representationalCoercion (cab :: Coercion a b) = let
        cn :: forall t. Coercion (LangList t) (Rec0 (Link t))
        cn = MkCoercion

        cl :: Coercion (Link a) (Link b)
        cl = case cab of
            MkCoercion -> MkCoercion
        in invert cn . rec0Representational cl . cn

instance MaybeRepresentational LangList where
    maybeRepresentational = Just Dict

instance HasVariance LangList where
    type VarianceOf LangList = 'Covariance

langListStoreAdapter :: forall a. StoreAdapter a -> StoreAdapter (LangList a)
langListStoreAdapter sa = coerce $ let
    sl :: StoreAdapter (Link a (Rec0 (Link a)))
    sl = linkStoreAdapter sa $ coerce sl
    in sl

listToLang :: [a] -> LangList a
listToLang [] = MkLangList $ MkRec0 NilLink
listToLang (a : aa) = MkLangList $ MkRec0 $ ConsLink a $ unLangList $ listToLang aa

langToList :: LangList a -> [a]
langToList (MkLangList (MkRec0 NilLink)) = []
langToList (MkLangList (MkRec0 (ConsLink a b))) = a : langToList (MkLangList b)

listLangIso :: Bijection [a] (LangList a)
listLangIso = MkIsomorphism listToLang langToList

-- LangList
instance HasQGroundType '[CoCCRVariance] LangList where
    qGroundType = let
        storability :: Storability '[CoCCRVariance] LangList
        storability = let
            stbKind = ConsListType Refl NilListType
            stbCovaryMap = covarymap
            stbAdapterExprKnot = pureStorabilityAdapter @LangList $ \(ConsArguments t NilArguments) -> langListStoreAdapter t
            in MkStorability{..}
        props = singleGroundProperty storabilityProperty storability
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangList)|]) "List."){qgtProperties = props}

-- []
instance
    forall (pshim :: PolyShimKind) polarity a.
    ( FromQIsoShim pshim
    , Is PolarityType polarity
    , HasQType pshim polarity a
    ) =>
    HasQType pshim polarity [a]
    where
    qType = isoMapQIsoShimWit (isoFunctionToShim "listLang" listLangIso) qType

---

newtype LangList1 a = MkLangList1 {unLangList1 :: (a, LangList a)}

instance RepresentationalRole LangList1 where
    representationalCoercion (cab :: Coercion a b) = let
        cn :: forall t. Coercion (LangList1 t) (t, LangList t)
        cn = MkCoercion

        cl :: Coercion (a, LangList a) (b, LangList b)
        cl = representationalCoercion2 cab $ representationalCoercion cab
        in invert cn . cl . cn

instance MaybeRepresentational LangList1 where
    maybeRepresentational = Just Dict

instance Functor LangList1 where
    fmap ab (MkLangList1 (a, la)) = MkLangList1 (ab a, fmap ab la)

instance HasVariance LangList1 where
    type VarianceOf LangList1 = 'Covariance

list1Codec :: Codec (LangList a) (LangList1 a)
list1Codec =
    MkCodec
        { encode = \(MkLangList1 (a, MkLangList (MkRec0 la))) -> MkLangList $ MkRec0 $ ConsLink a $ MkRec0 la
        , decode = \case
            MkLangList (MkRec0 (ConsLink a (MkRec0 la))) -> Just $ MkLangList1 (a, MkLangList $ MkRec0 la)
            _ -> Nothing
        }

langList1StoreAdapter :: forall a. StoreAdapter a -> StoreAdapter (LangList1 a)
langList1StoreAdapter sa = injectiveFilter list1Codec $ langListStoreAdapter sa

nonEmptyToLang :: NonEmpty a -> LangList1 a
nonEmptyToLang (a :| aa) = MkLangList1 (a, listToLang aa)

langToNonEmpty :: LangList1 a -> NonEmpty a
langToNonEmpty (MkLangList1 (a, aa)) = a :| langToList aa

decodeToLangList1 :: LangList a -> Maybe (LangList1 a)
decodeToLangList1 (MkLangList (MkRec0 l)) = case l of
    ConsLink a b -> Just $ MkLangList1 $ (a, MkLangList b)
    NilLink -> Nothing

-- LangList1
instance HasQGroundType '[CoCCRVariance] LangList1 where
    qGroundType = let
        storability :: Storability '[CoCCRVariance] LangList1
        storability = let
            stbKind = ConsListType Refl NilListType
            stbCovaryMap = covarymap
            stbAdapterExprKnot = pureStorabilityAdapter @LangList1 $ \(ConsArguments t NilArguments) -> langList1StoreAdapter t
            in MkStorability{..}
        props = singleGroundProperty storabilityProperty storability
        gds :: QPolyGreatestDynamicSupertype '[CoCCRVariance] LangList1
        gds =
            MkPolyGreatestDynamicSupertype
                (ConsCCRArguments (CoNonpolarArgument $ mkTypeVarT @"a") NilCCRArguments)
                $ mapNegShimWit (functionToShim "decodeToLangList1" $ decodeToLangList1 . coercionToFunction (applyCoercion1 id MkCoercion)) (qGroundedType :: _ (LangList A))
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangList1)|]) "List1.")
            { qgtProperties = props
            , qgtGreatestDynamicSupertype = gds
            }

-- NonEmpty
instance
    forall (pshim :: PolyShimKind) polarity a.
    ( FromQIsoShim pshim
    , Is PolarityType polarity
    , HasQType pshim polarity a
    ) =>
    HasQType pshim polarity (NonEmpty a)
    where
    qType = mapQIsoShimWit (functionToShim "nonEmptyToLang" nonEmptyToLang) (functionToShim "langToNonEmpty" langToNonEmpty) qType
