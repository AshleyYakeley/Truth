module Pinafore.Base.PredicateMorphism
    ( HasPinaforeEntityUpdate
    , propertyMorphism
    ) where

import Pinafore.Base.Edit
import Pinafore.Base.EntityAdapter
import Pinafore.Base.Know
import Pinafore.Base.Lens
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

predicatePinaforeTableLensMorphism ::
       forall a b. EntityAdapter a -> EntityAdapter b -> Predicate -> PinaforeLensMorphism PinaforeEntityUpdate a a b b
predicatePinaforeTableLensMorphism (MkEntityAdapter ap aget aput) (MkEntityAdapter bp bget bput) prd = let
    pmGet :: a -> ReadM PinaforeEntityRead (Know b)
    pmGet a = do
        valp <- readM $ PinaforeEntityReadGetProperty prd $ ap a
        bget valp readM
    pmBaseUpdate :: PinaforeEntityUpdate -> ReadM PinaforeEntityRead Bool
    pmBaseUpdate (MkEditUpdate (PinaforeEntityEditSetPredicate p _ _))
        | p == prd = return True
    pmBaseUpdate _ = return False
    pmPut :: Know a -> Know b -> ReadM PinaforeEntityRead (Maybe ([PinaforeEntityEdit], Maybe (Know a)))
    pmPut (Known subja) kvalb = do
        aedits <- aput subja readM
        mbedits <- for kvalb $ \valb -> bput valb readM
        let edits = aedits <> (fromKnow [] mbedits) <> [PinaforeEntityEditSetPredicate prd (ap subja) (fmap bp kvalb)]
        return $ Just (edits, Nothing)
    pmPut Unknown _ = return Nothing
    pmInv :: b -> ReadM PinaforeEntityRead [a]
    pmInv valb = do
        setp <- readM $ PinaforeEntityReadLookupPredicate prd $ bp valb
        setka <- for (setToList setp) $ \p -> aget p readM
        return $ catKnowns setka
    in MkPinaforeLensMorphism {..}

propertyMorphism ::
       HasPinaforeEntityUpdate baseupdate
    => EntityAdapter a
    -> EntityAdapter b
    -> Predicate
    -> PinaforeLensMorphism baseupdate a a b b
propertyMorphism pa pb prd =
    mapPinaforeLensMorphismBase (baseChangeLens @PinaforeEntityUpdate) $ predicatePinaforeTableLensMorphism pa pb prd
