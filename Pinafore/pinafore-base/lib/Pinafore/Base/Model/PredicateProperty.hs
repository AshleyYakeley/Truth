module Pinafore.Base.Model.PredicateProperty
    ( predicateProperty
    ) where

import Changes.Core
import Pinafore.Base.Edit
import Pinafore.Base.Know
import Pinafore.Base.Model.FunctionAttribute
import Pinafore.Base.Model.LensAttribute
import Pinafore.Base.Model.LensProperty
import Pinafore.Base.Storable.EntityStorer
import Pinafore.Base.Storable.StoreAdapter
import Shapes

predicateProperty ::
       forall a b.
       StoreAdapter Identity a
    -> StoreAdapter Identity b
    -> Predicate
    -> StorageLensProperty a a b b QStorageUpdate
predicateProperty eaa eab prd = let
    ap = storeAdapterConvert eaa
    bp = storeAdapterConvert eab
    sfaRead :: a -> ReadM QStorageRead (Know b)
    sfaRead a = do
        valp <- readM $ QStorageReadGet eaa prd a
        readM $ QStorageReadEntity eab valp
    sfaUpdate :: QStorageUpdate -> ReadM QStorageRead (Maybe (a -> ReadM QStorageRead (Maybe (Know b))))
    sfaUpdate (MkQStorageUpdate p s kv)
        | p == prd =
            return $
            Just $ \a ->
                case ap a == s of
                    False -> return Nothing
                    True -> do
                        kb <- for kv $ \b -> readM $ QStorageReadEntity eab b
                        return $ Just $ exec kb
    sfaUpdate _ = return Nothing
    slaFunction = MkStorageFunctionAttribute {..}
    slaPut :: Know a -> Know b -> ReadM QStorageRead (Maybe ([QStorageEdit], Maybe (Know a)))
    slaPut (Known subja) kvalb = return $ Just ([MkQStorageEdit eaa eab prd subja kvalb], Nothing)
    slaPut Unknown _ = return Nothing
    slpAttribute = MkStorageLensAttribute {..}
    slpInvGet :: b -> ReadM QStorageRead [a]
    slpInvGet valb = do
        setp <- readM $ QStorageReadLookup prd $ bp valb
        setka <- for (setToList setp) $ \p -> readM $ QStorageReadEntity eaa p
        return $ catKnowns setka
    slpInvBaseUpdate :: QStorageUpdate -> ReadM QStorageRead (Maybe (b -> ReadM QStorageRead [(Bool, a)]))
    slpInvBaseUpdate (MkQStorageUpdate p s kv)
        | p == prd =
            return $
            Just $ \b -> do
                ka <- readM $ QStorageReadEntity eaa s
                return $
                    case ka of
                        Known a -> [(kv == Known (bp b), a)]
                        Unknown -> []
    slpInvBaseUpdate _ = return Nothing
    in MkStorageLensProperty {..}
