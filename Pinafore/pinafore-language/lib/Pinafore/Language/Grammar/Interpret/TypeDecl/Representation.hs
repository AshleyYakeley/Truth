module Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
    ( EqWitness
    , leftCodec
    , extendRightCodec
    , TypeRepresentation(..)
    , getEnumTypeRepresentation
    , getListEnumTypeRepresentation
    , getWitnessTypeRepresentation
    , getWitnessTypeRepresentationEq
    , getEitherTypeRepresentation
    , getEitherTypeRepresentationEq
    , getPreferredTypeRepresentation
    , getPreferredTypeRepresentationEq
    ) where

import Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
import Shapes

type TypeRepresentation :: [Type] -> Type -> Type
data TypeRepresentation lt t = MkTypeRepresentation
    { trConstructors :: ListType (Codec t) lt
    , trVarMapping :: VarMapping t
    }

enumTypeRepresentation :: ListType ((:~:) ()) lt -> TypeRepresentation lt Int
enumTypeRepresentation lt = let
    getCodec :: forall t. (() :~: t) -> StateT Int Identity (Codec' Maybe Int t)
    getCodec Refl = do
        i <- get
        put $ succ i
        return $
            MkCodec
                (\i' ->
                     if i == i'
                         then Just ()
                         else Nothing)
                (\() -> i)
    in MkTypeRepresentation (evalState (mapMListType getCodec lt) 0) mempty

getEnumTypeRepresentation :: ListType ((:~:) ()) lt -> (forall t. (Eq t, Ord t) => TypeRepresentation lt t -> r) -> r
getEnumTypeRepresentation lt call = call $ enumTypeRepresentation lt

getListEnumTypeRepresentation ::
       forall w lt r. ListType (HListWit w) lt -> (forall t. (Eq t, Ord t) => TypeRepresentation lt t -> r) -> Maybe r
getListEnumTypeRepresentation lt call = do
    let
        getEU :: forall t. HListWit w t -> Maybe (() :~: t)
        getEU (MkHListWit NilListType) = Just Refl
        getEU _ = Nothing
    ltu <- mapMListType getEU lt
    return $ getEnumTypeRepresentation ltu call

witnessRepCodec :: TestEquality ow => ow a -> Codec (AnyValue ow) a
witnessRepCodec wit = MkCodec (matchAnyValue wit) (MkAnyValue wit)

witnessTypeRepresentation ::
       forall w lt (ow :: Type -> Type) m. (HasVarMapping w, TestEquality ow, Monad m)
    => (forall a. w a -> m (ow a))
    -> ListType w lt
    -> m (TypeRepresentation lt (AnyValue ow))
witnessTypeRepresentation newWit lt = do
    let
        getCodec :: forall t. w t -> WriterT (WitnessFDict ow VarMapping) m (Codec (AnyValue ow) t)
        getCodec wt = do
            wit <- lift $ newWit wt
            tell $ witnessFDictSingle wit $ getVarMapping wt
            return $ witnessRepCodec wit
    (codecs, fdict) <- runWriterT (mapMListType getCodec lt)
    return $
        MkTypeRepresentation codecs $
        MkVarMapping $ \v n -> do
            mdict <- witnessFDictMapM (\(MkVarMapping gm) -> gm v n) fdict
            return $
                dependentMapping $ \(MkAnyValue wit a) ->
                    MkDependentMapping a (MkAnyValue wit) $
                    fromMaybe (error "missing mapping") $ witnessFDictLookup wit mdict

getWitnessTypeRepresentation ::
       forall w lt r. HasVarMapping w
    => ListType w lt
    -> (forall t. TypeRepresentation lt t -> r)
    -> r
getWitnessTypeRepresentation lt call =
    runOW $ do
        tr <- witnessTypeRepresentation (\_ -> newOpenWitnessOW) lt
        return $ call tr

getWitnessTypeRepresentationEq ::
       forall w lt r. (WitnessConstraint Eq w, HasVarMapping w)
    => ListType w lt
    -> (forall t. Eq t => TypeRepresentation lt t -> r)
    -> r
getWitnessTypeRepresentationEq lt call =
    runOW $ do
        tr <-
            witnessTypeRepresentation
                (\wt -> do
                     ow <- newOpenWitnessOW
                     return $ MkPairType ow wt)
                lt
        return $ call tr

leftCodec :: Codec (Either b a) b
leftCodec = MkCodec eitherLeft Left

extendRightCodec :: Codec a b -> Codec (Either x a) b
extendRightCodec (MkCodec d e) = MkCodec (\xa -> eitherRight xa >>= d) (Right . e)

eitherTypeRepresentation :: HasVarMapping w => ListType w lt -> TypeRepresentation lt (ListSum lt)
eitherTypeRepresentation NilListType = MkTypeRepresentation NilListType pNone
eitherTypeRepresentation (ConsListType a1 ar) =
    case eitherTypeRepresentation ar of
        MkTypeRepresentation consr fvmapr ->
            MkTypeRepresentation (ConsListType leftCodec $ mapListType extendRightCodec consr) $
            getVarMapping a1 <+++> fvmapr

type EqWitness = Compose Dict Eq

getEitherTypeRepresentation :: HasVarMapping w => ListType w lt -> (forall t. TypeRepresentation lt t -> r) -> r
getEitherTypeRepresentation lt call = call $ eitherTypeRepresentation lt

getEitherTypeRepresentationEq ::
       (WitnessConstraint Eq w, HasVarMapping w)
    => ListType w lt
    -> (forall t. Eq t => TypeRepresentation lt t -> r)
    -> r
getEitherTypeRepresentationEq lt call =
    case listSumEq witnessConstraint lt of
        Dict -> call $ eitherTypeRepresentation lt

getPreferredTypeRepresentation ::
       forall w lt r. HasVarMapping w
    => ListType (HListWit w) lt
    -> (forall t. TypeRepresentation lt t -> r)
    -> r
getPreferredTypeRepresentation lt call =
    case getListEnumTypeRepresentation lt call of
        Nothing -> getWitnessTypeRepresentation lt call
        Just rt -> rt

getPreferredTypeRepresentationEq ::
       forall w lt r. (WitnessConstraint Eq w, HasVarMapping w)
    => ListType (HListWit w) lt
    -> (forall t. Eq t => TypeRepresentation lt t -> r)
    -> r
getPreferredTypeRepresentationEq lt call =
    case getListEnumTypeRepresentation lt call of
        Nothing -> getWitnessTypeRepresentationEq lt call
        Just rt -> rt
