module Pinafore.Base.PredicateMorphism
    ( propertyMorphism
    ) where

import Pinafore.Base.Edit
import Pinafore.Base.EntityAdapter
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

propertyMorphism ::
       forall a b. EntityAdapter a -> EntityAdapter b -> Predicate -> PinaforeLensMorphism PinaforeEntityUpdate a a b b
propertyMorphism (MkEntityAdapter ap aget aput) (MkEntityAdapter bp bget bput) prd = let
    pmGet :: a -> ReadM PinaforeEntityRead (Know b)
    pmGet a = do
        valp <- readM $ PinaforeEntityReadGetProperty prd $ ap a
        bget valp
    pmBaseUpdate :: PinaforeEntityUpdate -> Maybe (a -> ReadM PinaforeEntityRead (Maybe (Know b)))
    pmBaseUpdate (MkEditUpdate (PinaforeEntityEditSetPredicate p s kv))
        | p == prd =
            Just $ \a ->
                case ap a == s of
                    False -> return Nothing
                    True -> do
                        kb <- for kv $ \v -> bget v
                        return $ Just $ exec kb
    pmBaseUpdate _ = Nothing
    pmPut :: Know a -> Know b -> ReadM PinaforeEntityRead (Maybe ([PinaforeEntityEdit], Maybe (Know a)))
    pmPut (Known subja) kvalb = do
        aedits <- aput subja
        mbedits <- for kvalb $ \valb -> bput valb
        let edits = aedits <> (fromKnow [] mbedits) <> [PinaforeEntityEditSetPredicate prd (ap subja) (fmap bp kvalb)]
        return $ Just (edits, Nothing)
    pmPut Unknown _ = return Nothing
    pmInvGet :: b -> ReadM PinaforeEntityRead [a]
    pmInvGet valb = do
        setp <- readM $ PinaforeEntityReadLookupPredicate prd $ bp valb
        setka <- for (setToList setp) $ \p -> aget p
        return $ catKnowns setka
    pmInvBaseUpdate :: PinaforeEntityUpdate -> Maybe (b -> ReadM PinaforeEntityRead [(Bool, a)])
    pmInvBaseUpdate (MkEditUpdate (PinaforeEntityEditSetPredicate p s kv))
        | p == prd =
            Just $ \b -> do
                ka <- aget s
                return $
                    case ka of
                        Known a -> [(kv == Known (bp b), a)]
                        Unknown -> []
    pmInvBaseUpdate _ = Nothing
    in MkPinaforeLensMorphism {..}
