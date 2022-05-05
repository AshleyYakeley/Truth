module Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
    ( MaybeUnitWitness(..)
    , EqWitness
    , leftCodec
    , extendRightCodec
    , TypeRepresentation(..)
    , getEnumTypeRepresentation
    , getListEnumTypeRepresentation
    , getOpenWitnessTypeRepresentation
    , getOpenWitnessTypeRepresentationEq
    , getEitherTypeRepresentation
    , getEitherTypeRepresentationEq
    , getNaturalWitnessTypeRepresentation
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

getEnumTypeRepresentation ::
       ListType ((:~:) ()) lt
    -> (forall w' t. (TestEquality w', Eq t, Ord t) => TypeRepresentation lt t -> (t -> SomeOf w') -> r)
    -> r
getEnumTypeRepresentation lt call = call (enumTypeRepresentation lt) (\i -> MkSomeOf Refl i)

class MaybeUnitWitness w where
    maybeUnitWitness :: forall t. w t -> Maybe (() :~: t)

instance MaybeUnitWitness ((:~:) ()) where
    maybeUnitWitness Refl = Just Refl

instance MaybeUnitWitness (ListProductType w) where
    maybeUnitWitness (MkListProductType NilListType) = Just Refl
    maybeUnitWitness (MkListProductType (ConsListType _ _)) = Nothing

getListEnumTypeRepresentation ::
       forall w lt r. MaybeUnitWitness w
    => ListType w lt
    -> (forall w' t. (TestEquality w', Eq t, Ord t) => TypeRepresentation lt t -> (t -> SomeOf w') -> r)
    -> Maybe r
getListEnumTypeRepresentation lt call = do
    ltu <- mapMListType maybeUnitWitness lt
    return $ getEnumTypeRepresentation ltu call

witnessRepCodec :: TestEquality w => w a -> Codec (SomeOf w) a
witnessRepCodec wit = MkCodec (matchSomeOf wit) (MkSomeOf wit)

witnessTypeRepresentation ::
       forall w lt (ow :: Type -> Type) m. (HasVarMapping w, TestEquality ow, Monad m)
    => (forall a. w a -> m (ow a))
    -> ListType w lt
    -> m (TypeRepresentation lt (SomeOf ow))
witnessTypeRepresentation newWit lt = do
    let
        getCodec :: forall t. w t -> WriterT [SomeFor ow VarMapping] m (Codec (SomeOf ow) t)
        getCodec wt = do
            wit <- lift $ newWit wt
            tell $ pure $ MkSomeFor wit $ getVarMapping wt
            return $ witnessRepCodec wit
    (codecs, vmaps) <- runWriterT (mapMListType getCodec lt)
    return $ MkTypeRepresentation codecs $ dependentVarMapping vmaps

naturalWitnessTypeRepresentation ::
       forall w lt. HasVarMapping w
    => ListType w lt
    -> TypeRepresentation lt (SomeOf (ListElementType lt))
naturalWitnessTypeRepresentation lt =
    runIdentity $ witnessTypeRepresentation (\(MkPairType _ n) -> Identity n) $ pairListType lt $ countListType lt

getNaturalWitnessTypeRepresentation ::
       forall w lt r. HasVarMapping w
    => ListType w lt
    -> (forall w' t. TestEquality w' => TypeRepresentation lt t -> (t -> SomeOf w') -> r)
    -> r
getNaturalWitnessTypeRepresentation lt call = call (naturalWitnessTypeRepresentation lt) id

getOpenWitnessTypeRepresentation ::
       forall w lt r. HasVarMapping w
    => ListType w lt
    -> (forall w' t. TestEquality w' => TypeRepresentation lt t -> (t -> SomeOf w') -> r)
    -> r
getOpenWitnessTypeRepresentation lt call =
    runOW $ do
        tr <- witnessTypeRepresentation (\_ -> newOpenWitnessOW) lt
        return $ call tr id

getOpenWitnessTypeRepresentationEq ::
       forall w lt r. (WitnessConstraint Eq w, HasVarMapping w)
    => ListType w lt
    -> (forall w' t. (TestEquality w', Eq t) => TypeRepresentation lt t -> (t -> SomeOf w') -> r)
    -> r
getOpenWitnessTypeRepresentationEq lt call =
    runOW $ do
        tr <-
            witnessTypeRepresentation
                (\wt -> do
                     ow <- newOpenWitnessOW
                     return $ MkPairType ow wt)
                lt
        return $ call tr id

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
       forall w lt r. (MaybeUnitWitness w, HasVarMapping w)
    => ListType w lt
    -> (forall w' t. TestEquality w' => TypeRepresentation lt t -> (t -> SomeOf w') -> r)
    -> r
getPreferredTypeRepresentation lt call =
    case getListEnumTypeRepresentation lt call of
        Nothing -> getOpenWitnessTypeRepresentation lt call
        Just rt -> rt

getPreferredTypeRepresentationEq ::
       forall w lt r. (MaybeUnitWitness w, WitnessConstraint Eq w, HasVarMapping w)
    => ListType w lt
    -> (forall w' t. (TestEquality w', Eq t) => TypeRepresentation lt t -> (t -> SomeOf w') -> r)
    -> r
getPreferredTypeRepresentationEq lt call =
    case getListEnumTypeRepresentation lt call of
        Nothing -> getOpenWitnessTypeRepresentationEq lt call
        Just rt -> rt
