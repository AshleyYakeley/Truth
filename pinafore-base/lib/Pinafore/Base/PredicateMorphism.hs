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

predicatePinaforeMap ::
       forall a b.
       EntityAdapter a
    -> EntityAdapter b
    -> Predicate
    -> EditLens (ContextUpdate PinaforeEntityUpdate (WholeUpdate (Know a))) (WholeUpdate (Know b))
predicatePinaforeMap (MkEntityAdapter ap _ aput) (MkEntityAdapter bp bget bput) prd = let
    rfp :: ReadFunction (ContextUpdateReader PinaforeEntityUpdate (WholeUpdate (Know a))) PinaforeEntityRead
    rfp = tupleReadFunction SelectContext
    elGet :: ReadFunction (ContextUpdateReader PinaforeEntityUpdate (WholeUpdate (Know a))) (WholeReader (Know b))
    elGet mra ReadWhole = do
        ksubja <- mra $ MkTupleUpdateReader SelectContent ReadWhole
        case ksubja of
            Known subja -> do
                valp <- mra $ MkTupleUpdateReader SelectContext $ PinaforeEntityReadGetProperty prd $ ap subja
                bget valp $ rfp mra
            Unknown -> return Unknown
    elUpdate ::
           forall m. MonadIO m
        => ContextUpdate PinaforeEntityUpdate (WholeUpdate (Know a))
        -> Readable m (ContextUpdateReader PinaforeEntityUpdate (WholeUpdate (Know a)))
        -> m [WholeUpdate (Know b)]
    elUpdate (MkTupleUpdate SelectContext (MkEditUpdate (PinaforeEntityEditSetPredicate p s kvalp))) mra
        | p == prd = do
            ksubja <- mra $ MkTupleUpdateReader SelectContent ReadWhole
            if Known s == fmap ap ksubja
                then do
                    case kvalp of
                        Known valp -> do
                            kvalb <- bget valp $ rfp mra
                            return [MkWholeReaderUpdate kvalb]
                        Unknown -> return [MkWholeReaderUpdate Unknown]
                else return []
    elUpdate (MkTupleUpdate SelectContext _) _ = return []
    elUpdate (MkTupleUpdate SelectContent (MkWholeReaderUpdate ksubja)) mra =
        case ksubja of
            Known subja -> do
                valp <- mra $ MkTupleUpdateReader SelectContext $ PinaforeEntityReadGetProperty prd $ ap subja
                kb <- bget valp $ rfp mra
                return [MkWholeReaderUpdate kb]
            Unknown -> return [MkWholeReaderUpdate Unknown]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> Readable m (ContextUpdateReader PinaforeEntityUpdate (WholeUpdate (Know a)))
        -> m (Maybe [ContextUpdateEdit PinaforeEntityUpdate (WholeUpdate (Know a))])
    elPutEdits editbs mra =
        case lastWholeEdit editbs of
            Nothing -> return $ Just []
            Just kvalb -> do
                ksubja <- mra $ MkTupleUpdateReader SelectContent ReadWhole
                case ksubja of
                    Known subja -> do
                        aedits <- aput subja (rfp mra)
                        mbedits <- for kvalb $ \valb -> bput valb (rfp mra)
                        let
                            edits =
                                aedits <>
                                (fromKnow [] mbedits) <> [PinaforeEntityEditSetPredicate prd (ap subja) (fmap bp kvalb)]
                        return $ Just $ fmap (MkTupleUpdateEdit SelectContext) edits
                    Unknown ->
                        return $
                        case kvalb of
                            Known _ -> Nothing
                            Unknown -> Just []
    in MkEditLens {..}

predicateInverseFunction ::
       forall a b.
       EntityAdapter a
    -> EntityAdapter b
    -> Predicate
    -> PinaforeFunctionMorphism PinaforeEntityUpdate b [a]
predicateInverseFunction (MkEntityAdapter _ aget _) (MkEntityAdapter bp _ _) prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => Readable m PinaforeEntityRead
        -> b
        -> m [a]
    pfFuncRead mr valb = do
        setp <- mr $ PinaforeEntityReadLookupPredicate prd $ bp valb
        setka <- for (setToList setp) $ \p -> aget p mr
        return $ catKnowns setka
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeEntityUpdate
        -> Readable m PinaforeEntityRead
        -> m Bool
    pfUpdate (MkEditUpdate (PinaforeEntityEditSetPredicate p _ _)) _
        | p == prd = return True
    pfUpdate _ _ = return False
    in MkPinaforeFunctionMorphism {..}

predicatePinaforeTableLensMorphism ::
       EntityAdapter a -> EntityAdapter b -> Predicate -> PinaforeLensMorphism PinaforeEntityUpdate a b
predicatePinaforeTableLensMorphism pa pb prd =
    MkPinaforeLensMorphism (predicatePinaforeMap pa pb prd) (predicateInverseFunction pa pb prd)

propertyMorphism ::
       HasPinaforeEntityUpdate baseupdate
    => EntityAdapter a
    -> EntityAdapter b
    -> Predicate
    -> PinaforeLensMorphism baseupdate a b
propertyMorphism pa pb prd =
    mapPinaforeLensMorphismBase (baseEditLens @PinaforeEntityUpdate) $ predicatePinaforeTableLensMorphism pa pb prd
