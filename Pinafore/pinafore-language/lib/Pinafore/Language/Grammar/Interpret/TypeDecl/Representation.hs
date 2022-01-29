module Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
    ( leftCodec
    , extendRightCodec
    , TypeRepresentation(..)
    , enumTypeRepresentation
    , listEnumTypeRepresentation
    , witnessTypeRepresentation
    , eitherTypeRepresentation
    , preferredTypeRepresentation
    ) where

import Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
import Shapes

type TypeRepresentation :: [Type] -> Type
data TypeRepresentation lt =
    forall t. MkTypeRepresentation (ListType (Codec t) lt)
                                   (VarMapping t)

enumTypeRepresentation :: ListType ((:~:) ()) lt -> TypeRepresentation lt
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
    in MkTypeRepresentation @_ @Int (evalState (mapMListType getCodec lt) 0) mempty

listEnumTypeRepresentation :: forall w lt. ListType (HListWit w) lt -> Maybe (TypeRepresentation lt)
listEnumTypeRepresentation lt = do
    let
        getEU :: forall t. HListWit w t -> Maybe (() :~: t)
        getEU (MkHListWit NilListType) = Just Refl
        getEU _ = Nothing
    ltu <- mapMListType getEU lt
    return $ enumTypeRepresentation ltu

type WitnessRepresentation (s :: Type) = AnyValue (OpenWitness s)

witnessRepCodec :: OpenWitness s a -> Codec (WitnessRepresentation s) a
witnessRepCodec wit = MkCodec (matchAnyValue wit) (MkAnyValue wit)

witnessTypeRepresentation ::
       forall w lt. HasVarMapping w
    => ListType w lt
    -> TypeRepresentation lt
witnessTypeRepresentation lt = let
    mkTypeRepresentation :: forall (s :: Type). OW s (TypeRepresentation lt)
    mkTypeRepresentation = do
        let
            getCodec ::
                   forall t.
                   w t
                -> WriterT (WitnessFDict (OpenWitness s) VarMapping) (OW s) (Codec (WitnessRepresentation s) t)
            getCodec wt = do
                wit <- lift $ newOpenWitnessOW @s @t
                tell $ witnessFDictSingle wit $ getVarMapping wt
                return $ witnessRepCodec wit
        (codecs, fdict) <- runWriterT (mapMListType getCodec lt)
        return $
            MkTypeRepresentation @_ @(WitnessRepresentation s) codecs $
            MkVarMapping $ \v n -> do
                mdict <- witnessFDictMapM (\(MkVarMapping gm) -> gm v n) fdict
                return $
                    dependentMapping $ \(MkAnyValue wit a) ->
                        MkDependentMapping a (MkAnyValue wit) $
                        fromMaybe (error "missing mapping") $ witnessFDictLookup wit mdict
    in runOW mkTypeRepresentation

leftCodec :: Codec (Either b a) b
leftCodec = MkCodec eitherLeft Left

extendRightCodec :: Codec a b -> Codec (Either x a) b
extendRightCodec (MkCodec d e) = MkCodec (\xa -> eitherRight xa >>= d) (Right . e)

eitherTypeRepresentation :: HasVarMapping w => ListType w lt -> TypeRepresentation lt
eitherTypeRepresentation NilListType = MkTypeRepresentation NilListType pNone
eitherTypeRepresentation (ConsListType a1 ar) =
    case eitherTypeRepresentation ar of
        MkTypeRepresentation consr fvmapr ->
            MkTypeRepresentation (ConsListType leftCodec $ mapListType extendRightCodec consr) $
            getVarMapping a1 <+++> fvmapr

preferredTypeRepresentation ::
       forall w lt. HasVarMapping w
    => ListType (HListWit w) lt
    -> TypeRepresentation lt
preferredTypeRepresentation lt =
    case listEnumTypeRepresentation lt of
        Nothing -> witnessTypeRepresentation lt
        Just rt -> rt
