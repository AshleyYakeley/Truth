module Data.ListSet
    ( ListSet
    , pattern EmptyListSet
    , pattern SingleListSet
    , listSetMap
    , listSetMapMaybe
    , listSetFor
    , listSetForF
    , ListMap
    , listSetToMap
    , listSetToMapM
    , listSetToMapFor
    , listMapKeysToSet
    , listMapToSet
    , listMapToList
    )
where

import Data.List qualified as List

import Data.Codec
import Data.Filterable
import Data.KeyContainer
import Shapes.Import hiding (deleteBy)

newtype ListSet a = MkListSet
    { unFiniteSet :: [a]
    }
    deriving newtype (Foldable, MonoFoldable, GrowingAppend)

pattern EmptyListSet :: ListSet a
pattern EmptyListSet = MkListSet []

pattern SingleListSet :: a -> ListSet a
pattern SingleListSet a = MkListSet [a]

instance Eq a => Semigroup (ListSet a) where
    (<>) = union

instance Eq a => Monoid (ListSet a) where
    mempty = MkListSet mempty

instance Invariant ListSet where
    invmap ab _ (MkListSet items) = MkListSet $ fmap ab items

instance Summable ListSet where
    rVoid = MkListSet []
    MkListSet p <+++> MkListSet q = MkListSet $ fmap Left p <> fmap Right q

instance Productable ListSet where
    rUnit = MkListSet [()]
    MkListSet p <***> MkListSet q = MkListSet $ liftA2 (,) p q

instance Show a => Show (ListSet a) where
    show (MkListSet aa) = show aa

type instance Element (ListSet a) = a

instance MonoFilterable (ListSet a) where
    ofilter f (MkListSet aa) = MkListSet $ ofilter f aa
    ofilterM f (MkListSet aa) = fmap MkListSet $ ofilterM f aa

instance InjectiveFilterable ListSet where
    injectiveFilter (MkCodec amb _) (MkListSet aa) = MkListSet $ mapMaybe amb aa

instance Eq a => SetContainer (ListSet a) where
    type ContainerKey (ListSet a) = a
    member key (MkListSet aa) = elem key aa
    notMember key set = not $ member key set
    union (MkListSet a) (MkListSet b) = MkListSet $ List.union a b
    difference (MkListSet a) (MkListSet b) = MkListSet $ a \\ b
    intersection (MkListSet a) (MkListSet b) = MkListSet $ List.intersect a b
    keys (MkListSet a) = a

instance Eq a => IsSet (ListSet a) where
    insertSet = insertItem
    deleteSet = deleteKey
    singletonSet = MkListSet . pure
    setFromList = MkListSet . nub
    setToList = keys

instance MonoPointed (ListSet a) where
    opoint = MkListSet . pure

instance Eq a => Lattice (ListSet a) where
    (/\) = intersection
    (\/) = union

instance Eq a => BoundedJoinSemiLattice (ListSet a) where
    bottom = mempty

instance Eq a => ItemContainer (ListSet a)

instance Eq a => KeyContainer (ListSet a) where
    itemKey a = a
    lookupItem key = List.find (\k -> k == key)
    insertItem e (MkListSet []) = MkListSet [e]
    insertItem e (MkListSet (a : aa))
        | e == a = MkListSet $ e : aa
    insertItem e (MkListSet (a : aa)) = MkListSet $ a : (unFiniteSet $ insertItem e $ MkListSet aa)
    deleteKey _ (MkListSet []) = MkListSet []
    deleteKey k (MkListSet (k' : aa))
        | k == k' = MkListSet $ aa
    deleteKey k (MkListSet (a : aa)) = MkListSet $ a : (unFiniteSet $ deleteKey k $ MkListSet aa)
    fromItemList = setFromList

instance (Eq key, Random key) => IONewItemKeyContainer (ListSet key) where
    newKeyContainerItem = randomIO

listSetMap ::
    forall a b.
    Eq b =>
    (a -> b) ->
    ListSet a ->
    ListSet b
listSetMap f = setFromList . fmap f . toList

listSetMapMaybe ::
    forall a b.
    Eq b =>
    (a -> Maybe b) ->
    ListSet a ->
    ListSet b
listSetMapMaybe f = setFromList . mapMaybe f . toList

listSetFor ::
    forall m a b.
    (Applicative m, Eq b) =>
    ListSet a ->
    (a -> m b) ->
    m (ListSet b)
listSetFor fs f = fmap setFromList $ for (toList fs) f

listSetForF ::
    forall m a b.
    (Applicative m, Eq b) =>
    ListSet a ->
    (a -> m (Maybe b)) ->
    m (ListSet b)
listSetForF fs f = fmap setFromList $ forf (toList fs) f

newtype ListMap i a
    = MkListMap [(i, a)]

mapListMap :: (i -> a -> b) -> ListMap i a -> ListMap i b
mapListMap iab (MkListMap aa) = MkListMap $ fmap (\(i, a) -> (i, iab i a)) aa

instance Functor (ListMap i) where
    fmap ab = mapListMap $ \_ -> ab

instance Foldable (ListMap i) where
    foldMap am (MkListMap iaa) = mconcat $ fmap (am . snd) iaa

instance Traversable (ListMap i) where
    traverse amb (MkListMap iaa) = fmap MkListMap $ for iaa $ \(i, a) -> fmap (\b -> (i, b)) (amb a)

type instance Element (ListMap _ a) = a

instance MonoFunctor (ListMap i a)

instance MonoFoldable (ListMap i a)

instance MonoTraversable (ListMap i a)

instance GrowingAppend (ListMap i a)

instance Eq i => Semigroup (ListMap i a) where
    (<>) = union

instance Eq i => Monoid (ListMap i a) where
    mempty = MkListMap []

cmpFst ::
    forall i a b.
    Eq i =>
    (i, a) ->
    (i, b) ->
    Bool
cmpFst (i1, _) (i2, _) = i1 == i2

instance Eq i => SetContainer (ListMap i a) where
    type ContainerKey (ListMap i a) = i
    member key (MkListMap iaa) = elem key $ fmap fst iaa
    notMember key set = not $ member key set
    union (MkListMap iaa) (MkListMap ibb) = MkListMap $ List.unionBy cmpFst iaa ibb
    difference = differenceMap
    intersection = intersectionMap
    keys (MkListMap iaa) = fmap fst iaa

deleteFirstsBy :: (b -> a -> Bool) -> [a] -> [b] -> [a]
deleteFirstsBy eq aa bb = foldl (\aa' b -> deleteFirst (eq b) aa') aa bb

instance Eq i => PolyMap (ListMap i) where
    differenceMap (MkListMap iaa) (MkListMap ibb) = MkListMap $ deleteFirstsBy cmpFst iaa ibb
    intersectionWithMap f (MkListMap iaa) (MkListMap ibb) =
        MkListMap $ do
            (ia, a) <- iaa
            (ib, b) <- ibb
            guard $ ia == ib
            pure (ia, f a b)
    intersectionMap = intersectionWithMap $ \a _ -> a

instance Eq i => IsMap (ListMap i a) where
    type MapValue (ListMap i a) = a
    lookup k (MkListMap iaa) = List.lookup k iaa
    insertMap k v (MkListMap iaa) = let
        doInsert [] = [(k, v)]
        doInsert ((i, _) : aa)
            | i == k = (i, v) : aa
        doInsert (ia : aa) = ia : doInsert aa
        in MkListMap $ doInsert iaa
    deleteMap i (MkListMap iaa) = MkListMap $ deleteFirst (\(i', _) -> i == i') iaa
    singletonMap i a = MkListMap [(i, a)]
    mapFromList iaa = MkListMap $ nubBy cmpFst iaa
    mapToList = listMapToList

instance Eq i => HasKeysSet (ListMap i a) where
    type KeySet (ListMap i a) = ListSet i
    keysSet = listMapKeysToSet

listSetToMap :: forall i a. (i -> a) -> ListSet i -> ListMap i a
listSetToMap ia (MkListSet ii) = MkListMap $ fmap (\i -> (i, ia i)) ii

listSetToMapM ::
    forall m i a.
    Applicative m =>
    (i -> m a) ->
    ListSet i ->
    m (ListMap i a)
listSetToMapM ima (MkListSet ii) = fmap MkListMap $ for ii $ \i -> fmap (\a -> (i, a)) $ ima i

listSetToMapFor ::
    forall m i a.
    Applicative m =>
    ListSet i ->
    (i -> m a) ->
    m (ListMap i a)
listSetToMapFor ls ima = listSetToMapM ima ls

listMapKeysToSet :: forall i a. ListMap i a -> ListSet i
listMapKeysToSet (MkListMap iaa) = MkListSet $ fmap fst iaa

listMapToSet :: forall i a. ListMap i a -> ListSet (i, a)
listMapToSet (MkListMap iaa) = MkListSet iaa

listMapToList :: forall i a. ListMap i a -> [(i, a)]
listMapToList (MkListMap iaa) = iaa
