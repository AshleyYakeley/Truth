module Pinafore.Base.Edit
    ( QStorageRead(..)
    , QStorageEdit(..)
    , QStorageUpdate(..)
    ) where

import Changes.Core
import Pinafore.Base.Entity
import Pinafore.Base.EntityAdapter
import Pinafore.Base.EntityStorer
import Pinafore.Base.Know
import Pinafore.Base.Lens
import Shapes

-- | Some of these reads may add to the database, but will always give consistent results between changes.
type QStorageRead :: Type -> Type
data QStorageRead t where
    QStorageReadGet :: EntityAdapter t -> Predicate -> t -> QStorageRead Entity
    QStorageReadLookup :: Predicate -> Entity -> QStorageRead (FiniteSet Entity)
    QStorageReadEntity :: EntityAdapter t -> Entity -> QStorageRead (Know t)

instance Show (QStorageRead t) where
    show (QStorageReadGet st p s) = "get " ++ show p ++ " of " ++ show (entityAdapterConvert st s)
    show (QStorageReadLookup p v) = "lookup " ++ show p ++ " for " ++ show v
    show (QStorageReadEntity _ e) = "fetch " ++ show e

instance AllConstraint Show QStorageRead where
    allConstraint = Dict

data QStorageEdit where
    MkQStorageEdit :: EntityAdapter s -> EntityAdapter v -> Predicate -> s -> Know v -> QStorageEdit -- pred subj kval

instance Floating QStorageEdit QStorageEdit

instance ApplicableEdit QStorageEdit where
    applyEdit (MkQStorageEdit est evt ep es (Known ev)) _ (QStorageReadGet rst rp rs)
        | ep == rp
        , entityAdapterConvert est es == entityAdapterConvert rst rs = return $ entityAdapterConvert evt ev
    applyEdit (MkQStorageEdit est _ ep es Unknown) _ (QStorageReadGet rst rp rs)
        | ep == rp
        , entityAdapterConvert est es == entityAdapterConvert rst rs = newEntity
    applyEdit (MkQStorageEdit est evt ep es (Known ev)) mr (QStorageReadLookup rp rv)
        | ep == rp
        , entityAdapterConvert evt ev == rv = do
            ss <- mr $ QStorageReadLookup rp rv
            return $ insertSet (entityAdapterConvert est es) ss
    applyEdit (MkQStorageEdit est _ ep es Unknown) mr (QStorageReadLookup rp rv)
        | ep == rp = do
            ss <- mr $ QStorageReadLookup rp rv
            return $ deleteSet (entityAdapterConvert est es) ss
    applyEdit _ mr rt = mr rt

instance InvertibleEdit QStorageEdit where
    invertEdit (MkQStorageEdit st vt p s kv) mr = do
        oldentity <- mr $ QStorageReadGet st p s
        if fmap (entityAdapterConvert vt) kv == Known oldentity
            then return []
            else do
                kv' <- mr $ QStorageReadEntity vt oldentity
                return [MkQStorageEdit st vt p s kv']

type instance EditReader QStorageEdit = QStorageRead

instance Show QStorageEdit where
    show (MkQStorageEdit st vt p s kvt) =
        "set prop " ++
        show p ++ " of " ++ show (entityAdapterConvert st s) ++ " to " ++ show (fmap (entityAdapterConvert vt) kvt)

data QStorageUpdate =
    MkQStorageUpdate Predicate
                     Entity
                     (Know Entity)

type instance UpdateEdit QStorageUpdate = QStorageEdit

instance IsUpdate QStorageUpdate where
    editUpdate (MkQStorageEdit st vt p s kv) =
        MkQStorageUpdate p (entityAdapterConvert st s) (fmap (entityAdapterConvert vt) kv)

instance BaseChangeLens QStorageUpdate QStorageUpdate where
    baseChangeLens = id
