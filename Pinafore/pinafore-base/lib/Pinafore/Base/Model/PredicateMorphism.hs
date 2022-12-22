module Pinafore.Base.Model.PredicateMorphism
    ( propertyMorphism
    ) where

import Changes.Core
import Pinafore.Base.Edit
import Pinafore.Base.Know
import Pinafore.Base.Model.Morphism
import Pinafore.Base.Storable.EntityStorer
import Pinafore.Base.Storable.StoreAdapter
import Shapes

propertyMorphism ::
       forall a b. StoreAdapter a -> StoreAdapter b -> Predicate -> StorageLensMorphism a a b b QStorageUpdate
propertyMorphism eaa eab prd = let
    ap = storeAdapterConvert eaa
    bp = storeAdapterConvert eab
    pmGet :: a -> ReadM QStorageRead (Know b)
    pmGet a = do
        valp <- readM $ QStorageReadGet eaa prd a
        readM $ QStorageReadEntity eab valp
    pmBaseUpdate :: QStorageUpdate -> ReadM QStorageRead (Maybe (a -> ReadM QStorageRead (Maybe (Know b))))
    pmBaseUpdate (MkQStorageUpdate p s kv)
        | p == prd =
            return $
            Just $ \a ->
                case ap a == s of
                    False -> return Nothing
                    True -> do
                        kb <- for kv $ \b -> readM $ QStorageReadEntity eab b
                        return $ Just $ exec kb
    pmBaseUpdate _ = return Nothing
    pmPut :: Know a -> Know b -> ReadM QStorageRead (Maybe ([QStorageEdit], Maybe (Know a)))
    pmPut (Known subja) kvalb = return $ Just ([MkQStorageEdit eaa eab prd subja kvalb], Nothing)
    pmPut Unknown _ = return Nothing
    pmInvGet :: b -> ReadM QStorageRead [a]
    pmInvGet valb = do
        setp <- readM $ QStorageReadLookup prd $ bp valb
        setka <- for (setToList setp) $ \p -> readM $ QStorageReadEntity eaa p
        return $ catKnowns setka
    pmInvBaseUpdate :: QStorageUpdate -> ReadM QStorageRead (Maybe (b -> ReadM QStorageRead [(Bool, a)]))
    pmInvBaseUpdate (MkQStorageUpdate p s kv)
        | p == prd =
            return $
            Just $ \b -> do
                ka <- readM $ QStorageReadEntity eaa s
                return $
                    case ka of
                        Known a -> [(kv == Known (bp b), a)]
                        Unknown -> []
    pmInvBaseUpdate _ = return Nothing
    in MkStorageLensMorphism {..}
