module Pinafore.Language.Value.EntityMap where

import Import

newtype EntityMap (v :: Type) = MkEntityMap
    { unEntityMap :: HashMap Entity v
    }
    deriving newtype (Functor, Semigroup, Monoid, HasVariance)

instance RepresentationalRole EntityMap where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational EntityMap where
    maybeRepresentational = Just Dict

entityMapToList :: EntityMap v -> [(Entity, v)]
entityMapToList = mapToList . unEntityMap

entityMapFromList :: [(Entity, v)] -> EntityMap v
entityMapFromList = MkEntityMap . mapFromList

entityMapLookup :: Entity -> EntityMap v -> Maybe v
entityMapLookup k (MkEntityMap m) = lookup k m

entityMapInsert :: Entity -> v -> EntityMap v -> EntityMap v
entityMapInsert k v (MkEntityMap m) = MkEntityMap $ insertMap k v m

entityMapDelete :: Entity -> EntityMap v -> EntityMap v
entityMapDelete k (MkEntityMap m) = MkEntityMap $ deleteMap k m

entityMapSingle :: Entity -> v -> EntityMap v
entityMapSingle k v = MkEntityMap $ singletonMap k v

entityMapKeys :: EntityMap v -> [Entity]
entityMapKeys m = fmap fst $ entityMapToList m

entityMapValues :: EntityMap v -> [v]
entityMapValues m = fmap snd $ entityMapToList m
