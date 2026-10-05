module Pinafore.Base.Edit
    ( QStorageRead (..)
    , QStorageEdit (..)
    , QStorageUpdate (..)
    , overlayStorageLens
    )
where

import Changes.Core
import Shapes

import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.Storable.EntityStorer
import Pinafore.Base.Storable.StoreAdapter

type QStorageRead :: Type -> Type
data QStorageRead t where
    QStorageReadGet :: StoreAdapter t -> Predicate -> t -> QStorageRead (Know Entity)
    QStorageReadLookup :: Predicate -> Entity -> QStorageRead (ListSet Entity)
    QStorageReadEntity :: StoreAdapter t -> Entity -> QStorageRead (Know t)

instance Show (QStorageRead t) where
    show (QStorageReadGet st p s) = "get " ++ show p ++ " of " ++ show (storeAdapterConvert st s)
    show (QStorageReadLookup p v) = "lookup " ++ show p ++ " for " ++ show v
    show (QStorageReadEntity _ e) = "fetch " ++ show e

instance AllConstraint Show QStorageRead where
    allConstraint = Dict

data QStorageEdit where
    MkQStorageEdit :: StoreAdapter s -> StoreAdapter v -> Predicate -> s -> Know v -> QStorageEdit -- pred subj kval

instance FloatingOn QStorageEdit QStorageEdit

instance ApplicableEdit QStorageEdit where
    applyEdit (MkQStorageEdit est evt ep es (Known ev)) _ (QStorageReadGet rst rp rs)
        | ep == rp
        , storeAdapterConvert est es == storeAdapterConvert rst rs =
            return $ Known $ storeAdapterConvert evt ev
    applyEdit (MkQStorageEdit est _ ep es Unknown) _ (QStorageReadGet rst rp rs)
        | ep == rp
        , storeAdapterConvert est es == storeAdapterConvert rst rs =
            return Unknown
    applyEdit (MkQStorageEdit est evt ep es (Known ev)) mr (QStorageReadLookup rp rv)
        | ep == rp
        , storeAdapterConvert evt ev == rv = do
            ss <- mr $ QStorageReadLookup rp rv
            return $ insertSet (storeAdapterConvert est es) ss
    applyEdit (MkQStorageEdit est _ ep es Unknown) mr (QStorageReadLookup rp rv)
        | ep == rp = do
            ss <- mr $ QStorageReadLookup rp rv
            return $ deleteSet (storeAdapterConvert est es) ss
    applyEdit _ mr rt = mr rt

instance InvertibleEdit QStorageEdit where
    invertEdit (MkQStorageEdit st vt p s kv) mr = do
        koldentity <- mr $ QStorageReadGet st p s
        if fmap (storeAdapterConvert vt) kv == koldentity
            then return []
            else do
                kv' <- case koldentity of
                    Known oldentity -> mr $ QStorageReadEntity vt oldentity
                    Unknown -> return Unknown
                return [MkQStorageEdit st vt p s kv']

type instance EditReader QStorageEdit = QStorageRead

instance Show QStorageEdit where
    show (MkQStorageEdit st vt p s kvt) =
        "set prop "
            ++ show p
            ++ " of "
            ++ show (storeAdapterConvert st s)
            ++ " to "
            ++ show (fmap (storeAdapterConvert vt) kvt)

data QStorageUpdate
    = MkQStorageUpdate
        Predicate
        Entity
        (Know Entity)

type instance UpdateEdit QStorageUpdate = QStorageEdit

instance IsUpdate QStorageUpdate where
    editUpdate (MkQStorageEdit st vt p s kv) =
        MkQStorageUpdate p (storeAdapterConvert st s) (fmap (storeAdapterConvert vt) kv)

overlayStorageLens :: ChangeLens (PairUpdate QStorageUpdate QStorageUpdate) QStorageUpdate
overlayStorageLens = let
    clRead :: forall m. MonadIO m => Readable m (UpdateReader (PairUpdate QStorageUpdate QStorageUpdate)) -> Readable m QStorageRead
    clRead mr rt = let
        firstR :: Readable m QStorageRead
        firstR = mr . MkTupleUpdateReader SelectFirst
        secondR :: Readable m QStorageRead
        secondR = mr . MkTupleUpdateReader SelectSecond
        fallback :: forall t. QStorageRead (Know t) -> m (Know t)
        fallback query = do
            ka <- firstR query
            case ka of
                Known _ -> return ka
                Unknown -> secondR query
        in case rt of
            QStorageReadGet{} -> fallback rt
            QStorageReadEntity{} -> fallback rt
            QStorageReadLookup p _ -> do
                as <- firstR rt
                bs <- secondR rt
                -- A subject in the firstR store shadows every lower-store value.
                visible <- ofilterM (\s -> fmap (== Unknown) $ firstR $ QStorageReadGet plainStoreAdapter p s) bs
                return $ as <> visible
    clUpdate ::
        forall m.
        MonadIO m =>
        PairUpdate QStorageUpdate QStorageUpdate ->
        Readable m (UpdateReader (PairUpdate QStorageUpdate QStorageUpdate)) ->
        m [QStorageUpdate]
    clUpdate (MkTupleUpdate SelectFirst (MkQStorageUpdate p s kv)) mr = do
        value <- case kv of
            Known _ -> return kv
            Unknown -> mr $ MkTupleUpdateReader SelectSecond $ QStorageReadGet plainStoreAdapter p s
        return [MkQStorageUpdate p s value]
    clUpdate (MkTupleUpdate SelectSecond update@(MkQStorageUpdate p s _)) mr = do
        value <- mr $ MkTupleUpdateReader SelectFirst $ QStorageReadGet plainStoreAdapter p s
        return $ case value of
            Known _ -> []
            Unknown -> [update]
    clPutEdits :: forall m. MonadIO m => [QStorageEdit] -> Readable m (UpdateReader (PairUpdate QStorageUpdate QStorageUpdate)) -> m (Maybe [UpdateEdit (PairUpdate QStorageUpdate QStorageUpdate)])
    clPutEdits edits _ = return $ Just $ fmap (MkTupleUpdateEdit SelectFirst) edits
    in MkChangeLens{..}
