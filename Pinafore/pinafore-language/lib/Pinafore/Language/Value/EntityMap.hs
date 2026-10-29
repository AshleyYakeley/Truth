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

entityMapListIso :: Bijection [(Entity, t)] (EntityMap t)
entityMapListIso = MkIsomorphism entityMapFromList entityMapToList

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

entityMapMember :: Entity -> EntityMap v -> Bool
entityMapMember k (MkEntityMap m) = member k m

entityMapContains :: EntityMap v -> Entity -> Bool
entityMapContains em k = entityMapMember k em

newtype EntitySet = MkEntitySet {unEntitySet :: EntityMap ()} deriving newtype (Semigroup, Monoid)

entitySetToList :: EntitySet -> [Entity]
entitySetToList = entityMapKeys . unEntitySet

entitySetFromList :: [Entity] -> EntitySet
entitySetFromList = MkEntitySet . entityMapFromList . fmap (\e -> (e, ()))

entitySetListIso :: Bijection [Entity] EntitySet
entitySetListIso = MkIsomorphism entitySetFromList entitySetToList

entitySetInsert :: Entity -> EntitySet -> EntitySet
entitySetInsert k (MkEntitySet em) = MkEntitySet $ entityMapInsert k () em

entitySetDelete :: Entity -> EntitySet -> EntitySet
entitySetDelete k (MkEntitySet em) = MkEntitySet $ entityMapDelete k em

entitySetSingle :: Entity -> EntitySet
entitySetSingle k = MkEntitySet $ entityMapSingle k ()

entitySetMember :: Entity -> EntitySet -> Bool
entitySetMember k (MkEntitySet em) = entityMapMember k em

entitySetContains :: EntitySet -> Entity -> Bool
entitySetContains es k = entitySetMember k es
