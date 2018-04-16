module Truth.Core.Edit.Cache where

import Truth.Core.Edit.Edit
import Truth.Core.Import
import Truth.Core.Read

class IsCache (cache :: (* -> *) -> *) where
    cacheEmpty :: cache k
    cacheLookup :: TestEquality k => k t -> cache k -> Maybe t
    cacheTraverse :: Applicative m => (forall t. k t -> t -> m (Maybe t)) -> cache k -> m (cache k)
    cacheModify :: TestEquality k => k t -> (Maybe t -> (Maybe t, r)) -> cache k -> (cache k, r)

cacheModify_ :: (IsCache cache, TestEquality k) => k t -> (Maybe t -> Maybe t) -> cache k -> cache k
cacheModify_ key f cache = fst $ cacheModify key (\mt -> (f mt, ())) cache

cacheAdd :: (IsCache cache, TestEquality k) => k t -> t -> cache k -> cache k
cacheAdd k t cache = cacheModify_ k (\_ -> Just t) cache

newtype ListCache k =
    MkListCache [Any k]

instance IsCache ListCache where
    cacheEmpty = MkListCache []
    cacheLookup key (MkListCache cc) = listToMaybe $ mapMaybe (matchAny key) cc
    cacheTraverse f (MkListCache cc) =
        fmap (MkListCache . catMaybes) $ for cc $ \(MkAny key oldval) -> fmap (fmap $ MkAny key) $ f key oldval
    cacheModify key f (MkListCache cc) = let
        go [] =
            case f Nothing of
                (Nothing, r) -> (MkListCache [], r)
                (Just val, r) -> (MkListCache [MkAny key val], r)
        go (MkAny key' val:vv)
            | Just Refl <- testEquality key key' =
                case f $ Just val of
                    (Nothing, r) -> (MkListCache vv, r)
                    (Just newval, r) -> (MkListCache $ MkAny key' newval : vv, r)
        go (v:vv) =
            case go vv of
                (MkListCache vv', r) -> (MkListCache $ v : vv', r)
        in go cc

class CacheableEdit (edit :: *) where
    type EditCacheKey (cache :: (* -> *) -> *) edit :: * -> *
    editCacheAdd ::
           forall cache t. IsCache cache
        => EditReader edit t
        -> t
        -> cache (EditCacheKey cache edit)
        -> cache (EditCacheKey cache edit)
    editCacheLookup ::
           forall cache t. IsCache cache
        => EditReader edit t
        -> cache (EditCacheKey cache edit)
        -> Maybe t
    editCacheUpdate ::
           forall cache. IsCache cache
        => edit
        -> cache (EditCacheKey cache edit)
        -> IO (cache (EditCacheKey cache edit))
    -- defaults
    type EditCacheKey cache edit = EditReader edit
    default editCacheAdd :: forall cache t. ( IsCache cache
                                            , EditCacheKey cache edit ~ EditReader edit
                                            , TestEquality (EditReader edit)
                                            ) =>
                                                EditReader edit t -> t -> cache (EditCacheKey cache edit) -> cache (EditCacheKey cache edit)
    editCacheAdd = cacheAdd
    default editCacheLookup :: forall cache t. ( IsCache cache
                                               , EditCacheKey cache edit ~ EditReader edit
                                               , TestEquality (EditReader edit)
                                               ) =>
                                                   EditReader edit t -> cache (EditCacheKey cache edit) -> Maybe t
    editCacheLookup = cacheLookup
    default editCacheUpdate :: ( IsCache cache
                               , ApplicableEdit edit
                               , EditCacheKey cache edit ~ EditReader edit
                               , TestEquality (EditReader edit)
                               ) =>
                                  edit -> cache (EditCacheKey cache edit) -> IO (cache (EditCacheKey cache edit))
    editCacheUpdate edit =
        cacheTraverse $ \rt val -> let
            tmr :: MutableRead (ComposeM Maybe IO) (EditReader edit)
            tmr rt' =
                liftInner $ do
                    Refl <- testEquality rt rt'
                    return val
            in getComposeM $ applyEdit edit tmr rt
