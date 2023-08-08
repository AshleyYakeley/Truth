module Pinafore.Language.Value.Map where

import Data.Shim
import Pinafore.Base
import Shapes

newtype LangMap (v :: Type) = MkLangMap
    { unLangMap :: Map Entity v
    } deriving newtype (Functor, Semigroup, Monoid, HasVariance)

instance RepresentationalRole LangMap where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational LangMap where
    maybeRepresentational = Just Dict

langMapToList :: LangMap v -> [(Entity, v)]
langMapToList = mapToList . unLangMap

langMapFromList :: [(Entity, v)] -> LangMap v
langMapFromList = MkLangMap . mapFromList

langMapLookup :: Entity -> LangMap v -> Maybe v
langMapLookup k (MkLangMap m) = lookup k m

langMapInsert :: Entity -> v -> LangMap v -> LangMap v
langMapInsert k v (MkLangMap m) = MkLangMap $ insertMap k v m

langMapDelete :: Entity -> LangMap v -> LangMap v
langMapDelete k (MkLangMap m) = MkLangMap $ deleteMap k m

langMapSingle :: Entity -> v -> LangMap v
langMapSingle k v = MkLangMap $ singletonMap k v

langMapKeys :: LangMap v -> [Entity]
langMapKeys m = fmap fst $ langMapToList m

langMapValues :: LangMap v -> [v]
langMapValues m = fmap snd $ langMapToList m
