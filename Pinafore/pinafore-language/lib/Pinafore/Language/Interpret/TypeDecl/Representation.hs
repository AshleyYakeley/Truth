module Pinafore.Language.Interpret.TypeDecl.Representation
    ( MaybeUnitWitness (..)
    , EqWitness
    , leftCodec
    , extendRightCodec
    , TypeRepresentation (..)
    , getListEnumTypeRepresentation
    , getOpenWitnessTypeRepresentation
    , getOpenWitnessTypeRepresentationEq
    , getEitherTypeRepresentation
    , getEitherTypeRepresentationEq
    , getNaturalWitnessTypeRepresentation
    , getPreferredTypeRepresentation
    , getPreferredTypeRepresentationEq
    )
where

import Import

type TypeRepresentation :: [Type] -> Type -> Type
data TypeRepresentation lt t = MkTypeRepresentation
    { trConstructors :: ListType (Codec t) lt
    , trVarMapping :: VarMapping t
    }

class MaybeUnitWitness w where
    maybeUnitWitness :: forall t. w t -> Maybe (Dict (Singular t))

instance Singular t => MaybeUnitWitness ((:~:) t) where
    maybeUnitWitness Refl = Just Dict

instance MaybeUnitWitness (ListProductType w) where
    maybeUnitWitness (MkListProductType NilListType) = Just Dict
    maybeUnitWitness (MkListProductType (ConsListType _ _)) = Nothing

instance MaybeUnitWitness (ListVProductType w) where
    maybeUnitWitness (MkListVProductType (MkListVType lv :: ListVType w tt)) = do
        Refl <- listVectorIsEmpty lv
        Refl <- return $ emptyMapTypeRefl @Type @Type @w @tt
        return Dict

instance MaybeUnitWitness w => MaybeUnitWitness (Compose ((,) x) w) where
    maybeUnitWitness (Compose (_, wa)) = maybeUnitWitness wa

numberWitnesses :: Enum i => i -> ListType w lt -> ListType (Compose ((,) i) w) lt
numberWitnesses _ NilListType = NilListType
numberWitnesses i (ConsListType wa la) = ConsListType (Compose (i, wa)) $ numberWitnesses (succ i) la

getListEnumTypeRepresentation ::
    forall w lt r.
    MaybeUnitWitness w =>
    ListType w lt ->
    (forall t. (Eq t, Ord t) => TypeRepresentation lt t -> (t -> SomeOf (ListElementType lt)) -> r) ->
    Maybe r
getListEnumTypeRepresentation ltw call = do
    let
        toRep :: forall a. Compose ((,) Int) w a -> Maybe (Compose ((,) Int) (Compose Dict Singular) a)
        toRep (Compose (i, wa)) = do
            Dict <- maybeUnitWitness wa
            return $ Compose (i, Compose Dict)
    repltw <- mapMListType toRep $ numberWitnesses 0 ltw
    let
        codecFromRep :: forall a. Compose ((,) Int) (Compose Dict Singular) a -> Codec Int a
        codecFromRep (Compose (i, Compose Dict)) = invmap (\() -> single) (\_ -> ()) $ singleCodec i
        codecs :: ListType (Codec Int) lt
        codecs = mapListType codecFromRep repltw
        tf :: Int -> SomeOf (ListElementType lt)
        tf i = let
            sf =
                indexListElementType repltw
                    $ mapSome (fromMaybe (error "bad value") . peanoGreater (listTypeLengthType repltw))
                    $ valueToSome
                    $ naturalToPeano
                    $ fromIntegral i
            in case sf of
                MkSomeFor n (Compose (_, Compose Dict)) -> MkSomeOf n single
    return $ call (MkTypeRepresentation codecs mempty) tf

witnessRepCodec :: TestEquality w => w a -> Codec (SomeOf w) a
witnessRepCodec wit = MkCodec (matchSomeOf wit) (MkSomeOf wit)

witnessTypeRepresentation ::
    forall w lt (ow :: Type -> Type) m.
    (HasVarMapping w, TestOrder ow, Monad m) =>
    (forall a. w a -> m (ow a)) ->
    ListType w lt ->
    m (TypeRepresentation lt (SomeOf ow), WRaised ow (ListElementType lt))
witnessTypeRepresentation newWit ltw = do
    let
        getCodec ::
            forall t.
            PairType w (ListElementType lt) t ->
            WriterT (OrderedWitnessMapFor (ListElementType lt) ow, [SomeFor VarMapping ow]) m (Codec (SomeOf ow) t)
        getCodec (MkPairType wt et) = do
            wit <- lift $ newWit wt
            tell (orderedWitnessMapForSingle wit et, pure $ MkSomeFor wit $ getVarMapping wt)
            return $ witnessRepCodec wit
    (codecs, (tmap, vmaps)) <- runWriterT (mapMListType getCodec $ pairListType ltw $ countListType ltw)
    let
        tr :: TypeRepresentation lt (SomeOf ow)
        tr = MkTypeRepresentation codecs $ dependentVarMapping vmaps
        tw :: ow --> ListElementType lt
        tw owa = fromMaybe (error "bad value") $ orderedWitnessMapForLookup owa tmap
    return (tr, MkWRaised tw)

naturalWitnessTypeRepresentation ::
    forall w lt.
    HasVarMapping w =>
    ListType w lt ->
    TypeRepresentation lt (SomeOf (ListElementType lt))
naturalWitnessTypeRepresentation lt =
    fst $ runIdentity $ witnessTypeRepresentation (\(MkPairType _ n) -> Identity n) $ pairListType lt $ countListType lt

getNaturalWitnessTypeRepresentation ::
    forall w lt r.
    HasVarMapping w =>
    ListType w lt ->
    (forall w' t. TestEquality w' => TypeRepresentation lt t -> (t -> SomeOf w') -> r) ->
    r
getNaturalWitnessTypeRepresentation lt call = call (naturalWitnessTypeRepresentation lt) id

getOpenWitnessTypeRepresentation ::
    forall w lt r.
    HasVarMapping w =>
    ListType w lt ->
    (forall t. TypeRepresentation lt t -> (t -> SomeOf (ListElementType lt)) -> r) ->
    r
getOpenWitnessTypeRepresentation lt call =
    runOW $ do
        (tr, MkWRaised tf) <- witnessTypeRepresentation (\_ -> newOpenWitnessOW) lt
        return $ call tr $ mapSome tf

getOpenWitnessTypeRepresentationEq ::
    forall w lt r.
    (WitnessConstraint Eq w, HasVarMapping w) =>
    ListType w lt ->
    (forall t. Eq t => TypeRepresentation lt t -> (t -> SomeOf (ListElementType lt)) -> r) ->
    r
getOpenWitnessTypeRepresentationEq lt call =
    runOW $ do
        (tr, MkWRaised tf) <-
            witnessTypeRepresentation
                ( \wt -> do
                    ow <- newOpenWitnessOW
                    return $ MkPairType ow wt
                )
                lt
        return $ call tr $ mapSome tf

leftCodec :: Codec (Either b a) b
leftCodec = MkCodec eitherLeft Left

extendRightCodec :: Codec a b -> Codec (Either x a) b
extendRightCodec (MkCodec d e) = MkCodec (\xa -> eitherRight xa >>= d) (Right . e)

eitherTypeRepresentation :: HasVarMapping w => ListType w lt -> TypeRepresentation lt (ListSum lt)
eitherTypeRepresentation NilListType = MkTypeRepresentation NilListType rVoid
eitherTypeRepresentation (ConsListType a1 ar) =
    case eitherTypeRepresentation ar of
        MkTypeRepresentation consr fvmapr ->
            MkTypeRepresentation (ConsListType leftCodec $ mapListType extendRightCodec consr)
                $ getVarMapping a1
                <+++> fvmapr

type EqWitness = Compose Dict Eq

getEitherTypeRepresentation :: HasVarMapping w => ListType w lt -> (forall t. TypeRepresentation lt t -> r) -> r
getEitherTypeRepresentation lt call = call $ eitherTypeRepresentation lt

getEitherTypeRepresentationEq ::
    (WitnessConstraint Eq w, HasVarMapping w) =>
    ListType w lt ->
    (forall t. Eq t => TypeRepresentation lt t -> r) ->
    r
getEitherTypeRepresentationEq lt call =
    case listSumEq witnessConstraint lt of
        Dict -> call $ eitherTypeRepresentation lt

getPreferredTypeRepresentation ::
    forall w lt r.
    (MaybeUnitWitness w, HasVarMapping w) =>
    ListType w lt ->
    (forall t. TypeRepresentation lt t -> (t -> SomeOf (ListElementType lt)) -> r) ->
    r
getPreferredTypeRepresentation lt call =
    case getListEnumTypeRepresentation lt call of
        Nothing -> getOpenWitnessTypeRepresentation lt call
        Just rt -> rt

getPreferredTypeRepresentationEq ::
    forall w lt r.
    (MaybeUnitWitness w, WitnessConstraint Eq w, HasVarMapping w) =>
    ListType w lt ->
    (forall t. Eq t => TypeRepresentation lt t -> (t -> SomeOf (ListElementType lt)) -> r) ->
    r
getPreferredTypeRepresentationEq lt call =
    case getListEnumTypeRepresentation lt call of
        Nothing -> getOpenWitnessTypeRepresentationEq lt call
        Just rt -> rt
