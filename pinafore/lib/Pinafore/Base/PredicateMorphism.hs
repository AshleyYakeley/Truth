module Pinafore.Base.PredicateMorphism
    ( PointAdapter(..)
    , bijectionPointAdapter
    , HasPinaforePointEdit(..)
    , propertyMorphism
    ) where

import Pinafore.Base.Edit
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Pinafore.Base.Point
import Shapes
import Truth.Core

class HasPinaforePointEdit baseedit where
    pinaforePointLens :: EditLens baseedit PinaforePointEdit

instance HasPinaforePointEdit PinaforePointEdit where
    pinaforePointLens = id

data PointAdapter t = MkPointAdapter
    { pointAdapterConvert :: t -> Point
    , pointAdapterGet :: forall m. MonadIO m => Point -> MutableRead m PinaforePointRead -> m (Know t)
    , pointAdapterPut :: forall m. MonadIO m => t -> MutableRead m PinaforePointRead -> m [PinaforePointEdit]
    }

bijectionPointAdapter :: forall t. Bijection Point t -> PointAdapter t
bijectionPointAdapter (MkBijection pt tp) = let
    pointAdapterConvert = tp
    pointAdapterGet ::
           forall m. MonadIO m
        => Point
        -> MutableRead m PinaforePointRead
        -> m (Know t)
    pointAdapterGet p _ = return $ Known $ pt p
    pointAdapterPut _ _ = return []
    in MkPointAdapter {..}

predicatePinaforeMap ::
       forall a b.
       PointAdapter a
    -> PointAdapter b
    -> Predicate
    -> AnEditLens IdentityT (ContextEdit PinaforePointEdit (WholeEdit (Know a))) (WholeEdit (Know b))
predicatePinaforeMap (MkPointAdapter ap _ aput) (MkPointAdapter bp bget bput) prd = let
    rfp :: ReadFunction (ContextEditReader PinaforePointEdit (WholeEdit (Know a))) PinaforePointRead
    rfp = tupleReadFunction SelectContext
    efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforePointEdit (WholeEdit (Know a))) (WholeReader (Know b))
    efGet mra ReadWhole =
        lift $ do
            ksubja <- mra $ MkTupleEditReader SelectContent ReadWhole
            case ksubja of
                Known subja -> do
                    valp <- mra $ MkTupleEditReader SelectContext $ PinaforePointReadGetPredicate prd (ap subja)
                    bget valp $ rfp mra
                Unknown -> return Unknown
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforePointEdit (WholeEdit (Know a))
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know a)))
        -> IdentityT m [WholeEdit (Know b)]
    efUpdate (MkTupleEdit SelectContext (PinaforePointEditSetPredicate p s kvalp)) mra
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
                valp <- mra $ MkTupleEditReader SelectContext $ PinaforePointReadGetPredicate prd (ap subja)
                kb <- bget valp $ rfp mra
                return [MkWholeEdit kb]
            Unknown -> return [MkWholeEdit Unknown]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforePointEdit (WholeEdit (Know a))) (WholeEdit (Know b))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Know b)
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know a)))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit (Know a))])
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
                            (fromKnow [] mbedits) <> [PinaforePointEditSetPredicate prd (ap subja) (fmap bp kvalb)]
                    return $ Just $ fmap (MkTupleEdit SelectContext) edits
                Unknown ->
                    return $
                    case kvalb of
                        Known _ -> Nothing
                        Unknown -> Just []
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know b)]
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know a)))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit (Know a))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

predicateInverseFunction ::
       forall a b.
       PointAdapter a
    -> PointAdapter b
    -> Predicate
    -> APinaforeFunctionMorphism PinaforePointEdit IdentityT b (FiniteSet a)
predicateInverseFunction (MkPointAdapter _ aget _) (MkPointAdapter bp _ _) prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforePointRead
        -> b
        -> IdentityT m (FiniteSet a)
    pfFuncRead mr valb =
        lift $ do
            setp <- mr $ PinaforePointReadLookupPredicate prd (bp valb)
            setka <- for setp $ \p -> aget p mr
            return $ catKnowns setka
    pfUpdate ::
           forall m. MonadIO m
        => PinaforePointEdit
        -> MutableRead m PinaforePointRead
        -> IdentityT m Bool
    pfUpdate (PinaforePointEditSetPredicate p _ _) _
        | p == prd = return True
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

predicatePinaforeTableLensMorphism ::
       PointAdapter a -> PointAdapter b -> Predicate -> PinaforeLensMorphism PinaforePointEdit a b
predicatePinaforeTableLensMorphism pa pb prd =
    MkCloseUnlift identityUnlift $
    MkAPinaforeLensMorphism (predicatePinaforeMap pa pb prd) (predicateInverseFunction pa pb prd)

propertyMorphism ::
       HasPinaforePointEdit baseedit
    => PointAdapter a
    -> PointAdapter b
    -> Predicate
    -> PinaforeLensMorphism baseedit a b
propertyMorphism pa pb prd =
    mapPinaforeLensMorphismBase pinaforePointLens $ predicatePinaforeTableLensMorphism pa pb prd
