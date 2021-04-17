module Pinafore.Base.PredicateMorphism
    ( propertyMorphism
    ) where

import Changes.Core
import Pinafore.Base.Edit
import Pinafore.Base.EntityAdapter
import Pinafore.Base.EntityStorer
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes

propertyMorphism ::
       forall a b. EntityAdapter a -> EntityAdapter b -> Predicate -> PinaforeLensMorphism PinaforeStorageUpdate a a b b
propertyMorphism eaa eab prd = let
    ap = entityAdapterConvert eaa
    bp = entityAdapterConvert eab
    pmGet :: a -> ReadM PinaforeStorageRead (Know b)
    pmGet a = do
        valp <- readM $ PinaforeStorageReadGet eaa prd a
        readM $ PinaforeStorageReadEntity eab valp
    pmBaseUpdate ::
           PinaforeStorageUpdate -> ReadM PinaforeStorageRead (Maybe (a -> ReadM PinaforeStorageRead (Maybe (Know b))))
    pmBaseUpdate (MkPinaforeStorageUpdate p s kv)
        | p == prd =
            return $
            Just $ \a ->
                case ap a == s of
                    False -> return Nothing
                    True -> do
                        kb <- for kv $ \b -> readM $ PinaforeStorageReadEntity eab b
                        return $ Just $ exec kb
    pmBaseUpdate _ = return Nothing
    pmPut :: Know a -> Know b -> ReadM PinaforeStorageRead (Maybe ([PinaforeStorageEdit], Maybe (Know a)))
    pmPut (Known subja) kvalb = return $ Just ([MkPinaforeStorageEdit eaa eab prd subja kvalb], Nothing)
    pmPut Unknown _ = return Nothing
    pmInvGet :: b -> ReadM PinaforeStorageRead [a]
    pmInvGet valb = do
        setp <- readM $ PinaforeStorageReadLookup prd $ bp valb
        setka <- for (setToList setp) $ \p -> readM $ PinaforeStorageReadEntity eaa p
        return $ catKnowns setka
    pmInvBaseUpdate ::
           PinaforeStorageUpdate -> ReadM PinaforeStorageRead (Maybe (b -> ReadM PinaforeStorageRead [(Bool, a)]))
    pmInvBaseUpdate (MkPinaforeStorageUpdate p s kv)
        | p == prd =
            return $
            Just $ \b -> do
                ka <- readM $ PinaforeStorageReadEntity eaa s
                return $
                    case ka of
                        Known a -> [(kv == Known (bp b), a)]
                        Unknown -> []
    pmInvBaseUpdate _ = return Nothing
    in MkPinaforeLensMorphism {..}
