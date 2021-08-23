module Pinafore.Base.Edit
    ( PinaforeStorageRead(..)
    , PinaforeStorageEdit(..)
    , PinaforeStorageUpdate(..)
    ) where

import Changes.Core
import Pinafore.Base.Entity
import Pinafore.Base.EntityAdapter
import Pinafore.Base.EntityStorer
import Pinafore.Base.Know
import Pinafore.Base.Lens
import Shapes

-- | Some of these reads may add to the database, but will always give consistent results between changes.
type PinaforeStorageRead :: Type -> Type
data PinaforeStorageRead t where
    PinaforeStorageReadGet :: EntityAdapter t -> Predicate -> t -> PinaforeStorageRead Entity
    PinaforeStorageReadLookup :: Predicate -> Entity -> PinaforeStorageRead (FiniteSet Entity)
    PinaforeStorageReadEntity :: EntityAdapter t -> Entity -> PinaforeStorageRead (Know t)

instance Show (PinaforeStorageRead t) where
    show (PinaforeStorageReadGet st p s) = "get " ++ show p ++ " of " ++ show (entityAdapterConvert st s)
    show (PinaforeStorageReadLookup p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeStorageReadEntity _ e) = "fetch " ++ show e

instance AllWitnessConstraint Show PinaforeStorageRead where
    allWitnessConstraint = Dict

data PinaforeStorageEdit where
    MkPinaforeStorageEdit
        :: EntityAdapter s -> EntityAdapter v -> Predicate -> s -> Know v -> PinaforeStorageEdit -- pred subj kval

instance Floating PinaforeStorageEdit PinaforeStorageEdit

instance ApplicableEdit PinaforeStorageEdit where
    applyEdit (MkPinaforeStorageEdit est evt ep es (Known ev)) _ (PinaforeStorageReadGet rst rp rs)
        | ep == rp
        , entityAdapterConvert est es == entityAdapterConvert rst rs = return $ entityAdapterConvert evt ev
    applyEdit (MkPinaforeStorageEdit est _ ep es Unknown) _ (PinaforeStorageReadGet rst rp rs)
        | ep == rp
        , entityAdapterConvert est es == entityAdapterConvert rst rs = newEntity
    applyEdit (MkPinaforeStorageEdit est evt ep es (Known ev)) mr (PinaforeStorageReadLookup rp rv)
        | ep == rp
        , entityAdapterConvert evt ev == rv = do
            ss <- mr $ PinaforeStorageReadLookup rp rv
            return $ insertSet (entityAdapterConvert est es) ss
    applyEdit (MkPinaforeStorageEdit est _ ep es Unknown) mr (PinaforeStorageReadLookup rp rv)
        | ep == rp = do
            ss <- mr $ PinaforeStorageReadLookup rp rv
            return $ deleteSet (entityAdapterConvert est es) ss
    applyEdit _ mr rt = mr rt

instance InvertibleEdit PinaforeStorageEdit where
    invertEdit (MkPinaforeStorageEdit st vt p s kv) mr = do
        oldentity <- mr $ PinaforeStorageReadGet st p s
        if fmap (entityAdapterConvert vt) kv == Known oldentity
            then return []
            else do
                kv' <- mr $ PinaforeStorageReadEntity vt oldentity
                return [MkPinaforeStorageEdit st vt p s kv']

type instance EditReader PinaforeStorageEdit = PinaforeStorageRead

instance Show PinaforeStorageEdit where
    show (MkPinaforeStorageEdit st vt p s kvt) =
        "set prop " ++
        show p ++ " of " ++ show (entityAdapterConvert st s) ++ " to " ++ show (fmap (entityAdapterConvert vt) kvt)

data PinaforeStorageUpdate =
    MkPinaforeStorageUpdate Predicate
                            Entity
                            (Know Entity)

type instance UpdateEdit PinaforeStorageUpdate =
     PinaforeStorageEdit

instance IsUpdate PinaforeStorageUpdate where
    editUpdate (MkPinaforeStorageEdit st vt p s kv) =
        MkPinaforeStorageUpdate p (entityAdapterConvert st s) (fmap (entityAdapterConvert vt) kv)

instance BaseChangeLens PinaforeStorageUpdate PinaforeStorageUpdate where
    baseChangeLens = id
