module Pinafore.Base.Edit
    ( QStorageRead(..)
    , QStorageEdit(..)
    , QStorageUpdate(..)
    ) where

import Changes.Core
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.Storable.EntityStorer
import Pinafore.Base.Storable.StoreAdapter
import Shapes

-- | Some of these reads may add to the database, but will always give consistent results between changes.
type QStorageRead :: Type -> Type
data QStorageRead t where
    QStorageReadGet :: StoreAdapter t -> Predicate -> t -> QStorageRead Entity
    QStorageReadLookup :: Predicate -> Entity -> QStorageRead (FiniteSet Entity)
    QStorageReadEntity :: StoreAdapter t -> Entity -> QStorageRead (Know t)

instance Show (QStorageRead t) where
    show (QStorageReadGet st p s) = "get " ++ show p ++ " of " ++ show (storeAdapterConvert st s)
    show (QStorageReadLookup p v) = "lookup " ++ show p ++ " for " ++ show v
    show (QStorageReadEntity _ e) = "fetch " ++ show e

instance AllConstraint Show QStorageRead where
    allConstraint = Dict

data QStorageEdit where
    MkQStorageEdit :: StoreAdapter s -> StoreAdapter v -> Predicate -> s -> Know v -> QStorageEdit -- pred subj kval

instance Floating QStorageEdit QStorageEdit

instance ApplicableEdit QStorageEdit where
    applyEdit (MkQStorageEdit est evt ep es (Known ev)) _ (QStorageReadGet rst rp rs)
        | ep == rp
        , storeAdapterConvert est es == storeAdapterConvert rst rs = return $ storeAdapterConvert evt ev
    applyEdit (MkQStorageEdit est _ ep es Unknown) _ (QStorageReadGet rst rp rs)
        | ep == rp
        , storeAdapterConvert est es == storeAdapterConvert rst rs = newEntity
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
        oldentity <- mr $ QStorageReadGet st p s
        if fmap (storeAdapterConvert vt) kv == Known oldentity
            then return []
            else do
                kv' <- mr $ QStorageReadEntity vt oldentity
                return [MkQStorageEdit st vt p s kv']

type instance EditReader QStorageEdit = QStorageRead

instance Show QStorageEdit where
    show (MkQStorageEdit st vt p s kvt) =
        "set prop " ++
        show p ++ " of " ++ show (storeAdapterConvert st s) ++ " to " ++ show (fmap (storeAdapterConvert vt) kvt)

data QStorageUpdate =
    MkQStorageUpdate Predicate
                     Entity
                     (Know Entity)

type instance UpdateEdit QStorageUpdate = QStorageEdit

instance IsUpdate QStorageUpdate where
    editUpdate (MkQStorageEdit st vt p s kv) =
        MkQStorageUpdate p (storeAdapterConvert st s) (fmap (storeAdapterConvert vt) kv)
