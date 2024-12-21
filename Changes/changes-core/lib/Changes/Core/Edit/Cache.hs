module Changes.Core.Edit.Cache
    ( IsCache(..)
    , cacheAdd
    , subcacheModify
    , ListCache
    , CacheableEdit(..)
    , trimWithMap
    , editCacheUpdates
    , SimpleCacheKey(..)
    ) where

import Changes.Core.Edit.Edit
import Changes.Core.Import
import Changes.Core.Read

class IsCache (cache :: (Type -> Type) -> Type) where
    cacheEmpty :: cache k
    cacheLookup :: TestEquality k => k t -> cache k -> Maybe t
    cacheTraverse :: Applicative m => (forall t. k t -> t -> m (Maybe t)) -> StateT (cache k) m ()
    cacheModify :: (TestEquality k, Functor m) => k t -> StateT (Maybe t) m r -> StateT (cache k) m r

cacheAdd :: (IsCache cache, TestEquality k, Applicative m) => k t -> t -> StateT (cache k) m ()
cacheAdd k t = cacheModify k $ StateT $ \_ -> pure ((), Just t)

subcacheModify ::
       (IsCache cache, TestEquality k, Functor m) => k (cache k') -> StateT (cache k') m r -> StateT (cache k) m r
subcacheModify key st = cacheModify key $ lensStateT (defaultLens cacheEmpty) st

-- | not the best cache
newtype ListCache k =
    MkListCache [SomeOf k]

instance IsCache ListCache where
    cacheEmpty = MkListCache []
    cacheLookup key (MkListCache cc) = listToMaybe $ mapMaybe (matchSomeOf key) cc
    cacheTraverse f =
        StateT $ \(MkListCache cc) ->
            fmap (\newcc -> ((), MkListCache $ catMaybes newcc)) $
            for cc $ \(MkSomeOf key oldval) -> fmap (fmap $ MkSomeOf key) $ f key oldval
    cacheModify key f =
        StateT $ \(MkListCache cc) -> let
            go [] =
                fmap
                    (fmap
                         (\case
                              Nothing -> MkListCache []
                              Just val -> MkListCache [MkSomeOf key val])) $
                runStateT f Nothing
            go (MkSomeOf key' val:vv)
                | Just Refl <- testEquality key key' =
                    fmap
                        (fmap
                             (\case
                                  Nothing -> MkListCache vv
                                  Just newval -> MkListCache $ MkSomeOf key' newval : vv)) $
                    runStateT f $ Just val
            go (v:vv) = fmap (fmap (\(MkListCache vv') -> MkListCache $ v : vv')) $ go vv
            in go cc

class CacheableEdit (edit :: Type) where
    trimEdits :: [edit] -> [edit]
    type EditCacheKey (cache :: (Type -> Type) -> Type) edit :: Type -> Type
    editCacheAdd ::
           forall cache m t. (IsCache cache, Applicative m)
        => EditReader edit t
        -> t
        -> StateT (cache (EditCacheKey cache edit)) m ()
    editCacheLookup ::
           forall cache t. IsCache cache
        => EditReader edit t
        -> cache (EditCacheKey cache edit)
        -> Maybe t
    editCacheUpdate ::
           forall cache. IsCache cache
        => edit
        -> StateT (cache (EditCacheKey cache edit)) IO ()
    -- defaults
    type EditCacheKey _ edit = EditReader edit
    default editCacheAdd ::
        forall cache m t.
            (IsCache cache, Applicative m, EditCacheKey cache edit ~ EditReader edit, TestEquality (EditReader edit)) =>
                    EditReader edit t -> t -> StateT (cache (EditCacheKey cache edit)) m ()
    editCacheAdd = cacheAdd
    default editCacheLookup ::
        forall cache t.
            (IsCache cache, EditCacheKey cache edit ~ EditReader edit, TestEquality (EditReader edit)) =>
                    EditReader edit t -> cache (EditCacheKey cache edit) -> Maybe t
    editCacheLookup = cacheLookup
    default editCacheUpdate ::
        (IsCache cache, ApplicableEdit edit, EditCacheKey cache edit ~ EditReader edit, TestEquality (EditReader edit)) =>
                edit -> StateT (cache (EditCacheKey cache edit)) IO ()
    editCacheUpdate edit =
        cacheTraverse $ \rt val -> let
            tmr :: Readable (ComposeInner Maybe IO) (EditReader edit)
            tmr rt' =
                liftInner $ do
                    Refl <- testEquality rt rt'
                    return val
            in unComposeInner $ applyEdit edit tmr rt

trimWithMap' :: Ord k => (e -> k) -> [e] -> ([e], Map k ())
trimWithMap' _ [] = ([], mempty)
trimWithMap' ek (e:ee) = let
    (ee', m) = trimWithMap' ek ee
    k = ek e
    in case lookup k m of
           Nothing -> (e : ee', insertMap k () m)
           Just () -> (ee', m)

trimWithMap :: Ord k => (e -> k) -> [e] -> [e]
trimWithMap ek ee = fst $ trimWithMap' ek ee

editCacheUpdates ::
       forall edit cache. (CacheableEdit edit, IsCache cache)
    => NonEmpty edit
    -> StateT (cache (EditCacheKey cache edit)) IO ()
editCacheUpdates ee = for_ ee $ editCacheUpdate

data SimpleCacheKey (a :: Type) (b :: Type) (ct :: Type) where
    MkSimpleCacheKey :: a -> SimpleCacheKey a b b

instance Eq a => TestEquality (SimpleCacheKey a b) where
    testEquality (MkSimpleCacheKey a1) (MkSimpleCacheKey a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing
