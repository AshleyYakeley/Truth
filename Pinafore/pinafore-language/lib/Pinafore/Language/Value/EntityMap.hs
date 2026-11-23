module Pinafore.Language.Value.EntityMap where

import Import

newtype EntityMap (v :: Type) = MkEntityMap
    { unEntityMap :: HashMap Entity v
    }
    deriving newtype (Functor, Foldable, Semigroup, Monoid, HasVariance, MonoFoldable, GrowingAppend, IsMap)

instance Traversable EntityMap where
    traverse afb (MkEntityMap m) = fmap MkEntityMap $ traverse afb m

instance MonoFunctor (EntityMap v)

instance MonoTraversable (EntityMap v)

type instance Element (EntityMap v) = v

instance RepresentationalRole EntityMap where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational EntityMap where
    maybeRepresentational = Just Dict

instance ItemContainer (EntityMap v) where
    type Item (EntityMap v) = (Entity, v)

instance SetContainer (EntityMap v) where
    type ContainerKey (EntityMap v) = Entity
    member k (MkEntityMap m) = member k m
    notMember key set = not $ member key set
    union (MkEntityMap a) (MkEntityMap b) = MkEntityMap $ union a b
    difference (MkEntityMap a) (MkEntityMap b) = MkEntityMap $ difference a b
    intersection (MkEntityMap a) (MkEntityMap b) = MkEntityMap $ intersection a b
    keys m = fmap fst $ entityMapToList m

instance KeyContainer (EntityMap v)

entityMapToList :: EntityMap v -> [(Entity, v)]
entityMapToList = mapToList

entityMapFromList :: [(Entity, v)] -> EntityMap v
entityMapFromList = mapFromList

entityMapListIso :: Bijection [(Entity, t)] (EntityMap t)
entityMapListIso = MkIsomorphism mapFromList mapToList

entityMapLookup :: Entity -> EntityMap v -> Maybe v
entityMapLookup = lookup

entityMapInsert :: Entity -> v -> EntityMap v -> EntityMap v
entityMapInsert = insertMap

entityMapDelete :: Entity -> EntityMap v -> EntityMap v
entityMapDelete = deleteMap

entityMapSingle :: Entity -> v -> EntityMap v
entityMapSingle = singletonMap

entityMapKeys :: EntityMap v -> [Entity]
entityMapKeys = keys

entityMapValues :: EntityMap v -> [v]
entityMapValues m = fmap snd $ mapToList m

entityMapMember :: Entity -> EntityMap v -> Bool
entityMapMember = member

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
