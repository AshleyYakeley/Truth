module Pinafore.Base.PredicateMorphism
    ( EntityAdapter(..)
    , bijectionEntityAdapter
    , HasPinaforeEntityEdit(..)
    , propertyMorphism
    ) where

import Pinafore.Base.Edit
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core
import Truth.Debug.Object

class HasPinaforeEntityEdit baseedit where
    pinaforeEntityLens :: EditLens baseedit PinaforeEntityEdit

instance HasPinaforeEntityEdit PinaforeEntityEdit where
    pinaforeEntityLens = id

data EntityAdapter t = MkEntityAdapter
    { entityAdapterConvert :: t -> Entity
    , entityAdapterGet :: forall m. MonadIO m => Entity -> MutableRead m PinaforeEntityRead -> m (Know t)
    , entityAdapterPut :: forall m. MonadIO m => t -> MutableRead m PinaforeEntityRead -> m [PinaforeEntityEdit]
    }

bijectionEntityAdapter :: forall t. (Entity -> t) -> (t -> Entity) -> EntityAdapter t
bijectionEntityAdapter pt te = let
    entityAdapterConvert = te
    entityAdapterGet ::
           forall m. MonadIO m
        => Entity
        -> MutableRead m PinaforeEntityRead
        -> m (Know t)
    entityAdapterGet p _ = return $ Known $ pt p
    entityAdapterPut _ _ = return []
    in MkEntityAdapter {..}

predicatePinaforeMap ::
       forall a b.
       EntityAdapter a
    -> EntityAdapter b
    -> Predicate
    -> AnEditLens IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit (Know a))) (WholeEdit (Know b))
predicatePinaforeMap (MkEntityAdapter ap _ aput) (MkEntityAdapter bp bget bput) prd = traceAThing "predicatePinaforeMap" $ let
    rfp :: ReadFunction (ContextEditReader PinaforeEntityEdit (WholeEdit (Know a))) PinaforeEntityRead
    rfp = tupleReadFunction SelectContext
    efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforeEntityEdit (WholeEdit (Know a))) (WholeReader (Know b))
    efGet mra ReadWhole =
        lift $ do
            ksubja <- mra $ MkTupleEditReader SelectContent ReadWhole
            case ksubja of
                Known subja -> do
                    valp <- mra $ MkTupleEditReader SelectContext $ PinaforeEntityReadGetProperty prd $ ap subja
                    bget valp $ rfp mra
                Unknown -> return Unknown
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEntityEdit (WholeEdit (Know a))
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know a)))
        -> IdentityT m [WholeEdit (Know b)]
    efUpdate (MkTupleEdit SelectContext (PinaforeEntityEditSetPredicate p s kvalp)) mra
        | p == prd =
            lift $ do
                ksubja <- mra $ MkTupleEditReader SelectContent ReadWhole
                if Known s == fmap ap ksubja
                    then do
                        case kvalp of
                            Known valp -> do
                                kvalb <- bget valp $ rfp mra
                                return [MkWholeEdit kvalb]
                            Unknown -> return [MkWholeEdit Unknown]
                    else return []
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit ksubja)) mra =
        lift $
        case ksubja of
            Known subja -> do
                valp <- mra $ MkTupleEditReader SelectContext $ PinaforeEntityReadGetProperty prd $ ap subja
                kb <- bget valp $ rfp mra
                return [MkWholeEdit kb]
            Unknown -> return [MkWholeEdit Unknown]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit (Know a))) (WholeEdit (Know b))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Know b)
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know a)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit (Know a))])
    elPutEdit (MkWholeEdit kvalb) mra =
        lift $ do
            ksubja <- mra $ MkTupleEditReader SelectContent ReadWhole
            case ksubja of
                Known subja -> do
                    aedits <- aput subja (rfp mra)
                    mbedits <- for kvalb $ \valb -> bput valb (rfp mra)
                    let
                        edits =
                            aedits <>
                            (fromKnow [] mbedits) <> [PinaforeEntityEditSetPredicate prd (ap subja) (fmap bp kvalb)]
                    return $ Just $ fmap (MkTupleEdit SelectContext) edits
                Unknown ->
                    return $
                    case kvalb of
                        Known _ -> Nothing
                        Unknown -> Just []
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know a)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit (Know a))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

predicateInverseFunction ::
       forall a b.
       EntityAdapter a
    -> EntityAdapter b
    -> Predicate
    -> APinaforeFunctionMorphism PinaforeEntityEdit IdentityT b (FiniteSet a)
predicateInverseFunction (MkEntityAdapter _ aget _) (MkEntityAdapter bp _ _) prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeEntityRead
        -> b
        -> IdentityT m (FiniteSet a)
    pfFuncRead mr valb = traceBracket "predicateInverseFunction.pfFuncRead" $
        lift $ do
            setp <- mr $ PinaforeEntityReadLookupPredicate prd $ bp valb
            setka <- for setp $ \p -> aget p mr
            return $ catKnowns setka
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeEntityEdit
        -> MutableRead m PinaforeEntityRead
        -> IdentityT m Bool
    pfUpdate (PinaforeEntityEditSetPredicate p _ _) _
        | p == prd = traceBracket "predicateInverseFunction.pfUpdate: True" $ return True
    pfUpdate _ _ = traceBracket "predicateInverseFunction.pfUpdate: False" $ return False
    in MkAPinaforeFunctionMorphism {..}

predicatePinaforeTableLensMorphism ::
       EntityAdapter a -> EntityAdapter b -> Predicate -> PinaforeLensMorphism PinaforeEntityEdit a b
predicatePinaforeTableLensMorphism pa pb prd =
    MkCloseUnlift identityUnlift $
    MkAPinaforeLensMorphism (predicatePinaforeMap pa pb prd) (predicateInverseFunction pa pb prd)

propertyMorphism ::
       HasPinaforeEntityEdit baseedit
    => EntityAdapter a
    -> EntityAdapter b
    -> Predicate
    -> PinaforeLensMorphism baseedit a b
propertyMorphism pa pb prd =
    mapPinaforeLensMorphismBase pinaforeEntityLens $ predicatePinaforeTableLensMorphism pa pb prd
