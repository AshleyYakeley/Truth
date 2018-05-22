module Pinafore.PredicateMorphism
    ( HasPinaforeEntityEdit(..)
    , predicatePinaforeLensMorphism
    , literalPinaforeInverseFunctionMorphism
    , literalPinaforeLensMorphism
    ) where

import Pinafore.Entity
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Table (Predicate)
import Shapes
import Truth.Core
import Truth.Debug.Object

class HasPinaforeEntityEdit baseedit where
    pinaforePointLens :: EditLens baseedit PinaforeEntityEdit

instance HasPinaforeEntityEdit PinaforeEntityEdit where
    pinaforePointLens = id

predicatePinaforeMap ::
       Predicate -> AnEditLens IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit Point)) (WholeEdit Point)
predicatePinaforeMap prd = traceArgAThing "predicatePinaforeMap" $ let
    efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforeEntityEdit (WholeEdit Point)) (WholeReader Point)
    efGet mr ReadWhole =
        lift $ do
            subj <- mr $ MkTupleEditReader SelectContent ReadWhole
            mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadGetPredicate prd subj
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEntityEdit (WholeEdit Point)
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit Point))
        -> IdentityT m [WholeEdit Point]
    efUpdate (MkTupleEdit SelectContext (PinaforeEntityEditSetPredicate p s v)) mr
        | p == prd =
            lift $ do
                subj <- mr $ MkTupleEditReader SelectContent ReadWhole
                return $
                    if s == subj
                        then [MkWholeEdit v]
                        else []
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit subj)) mr =
        lift $ do
            val <- mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadGetPredicate prd subj
            return [MkWholeEdit val]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit Point)) (WholeEdit Point)
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit Point
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit Point)])
    elPutEdit (MkWholeEdit v) mr = do
        subj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        return $ Just [MkTupleEdit SelectContext $ PinaforeEntityEditSetPredicate prd subj v]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit Point]
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit Point)])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

predicateInverseFunction :: Predicate -> APinaforeFunctionMorphism PinaforeEntityEdit IdentityT Point (FiniteSet Point)
predicateInverseFunction prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeEntityRead
        -> Point
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforeEntityReadLookupPredicate prd val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeEntityEdit
        -> MutableRead m PinaforeEntityRead
        -> IdentityT m Bool
    pfUpdate (PinaforeEntityEditSetPredicate p _ _) _
        | p == prd = return True
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

predicatePinaforeTableLensMorphism :: Predicate -> PinaforeLensMorphism PinaforeEntityEdit Point Point
predicatePinaforeTableLensMorphism prd =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism (predicatePinaforeMap prd) (predicateInverseFunction prd)

predicatePinaforeLensMorphism ::
       HasPinaforeEntityEdit baseedit => Predicate -> PinaforeLensMorphism baseedit Point Point
predicatePinaforeLensMorphism prd =
    mapPinaforeLensMorphismBase pinaforePointLens $ predicatePinaforeTableLensMorphism prd

literalPinaforeMap ::
       forall t. AsLiteral t
    => AnEditLens IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit Point)) (WholeEdit (Maybe t))
literalPinaforeMap = let
    efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforeEntityEdit (WholeEdit Point)) (WholeReader (Maybe t))
    efGet mr ReadWhole =
        lift $ do
            subj <- mr $ MkTupleEditReader SelectContent ReadWhole
            mlit <- mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadToLiteral subj
            return $ mlit >>= fromLiteral
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEntityEdit (WholeEdit Point)
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit Point))
        -> IdentityT m [WholeEdit (Maybe t)]
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit subj)) mr =
        lift $ do
            mlit <- mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadToLiteral subj
            return [MkWholeEdit $ mlit >>= fromLiteral]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit Point)) (WholeEdit (Maybe t))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe t)
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit Point)])
    elPutEdit (MkWholeEdit (Just t)) mr =
        lift $ do
            p <- mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadFromLiteral t
            return $ Just [MkTupleEdit SelectContent $ MkWholeEdit p]
    elPutEdit (MkWholeEdit Nothing) _ = do
        e <- newPoint
        return $ Just [MkTupleEdit SelectContent $ MkWholeEdit e]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe t)]
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit Point)])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

literalInverseFunction ::
       forall t. AsLiteral t
    => APinaforeFunctionMorphism PinaforeEntityEdit IdentityT (Maybe t) Point
literalInverseFunction = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeEntityRead
        -> Maybe t
        -> IdentityT m Point
    pfFuncRead _ Nothing = newPoint
    pfFuncRead mr (Just val) = lift $ mr $ PinaforeEntityReadFromLiteral val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeEntityEdit
        -> MutableRead m PinaforeEntityRead
        -> IdentityT m Bool
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

literalPinaforeInverseFunctionMorphism ::
       (HasPinaforeEntityEdit baseedit, AsLiteral t) => PinaforeFunctionMorphism baseedit (Maybe t) Point
literalPinaforeInverseFunctionMorphism =
    mapPinaforeFunctionMorphismBase (editLensFunction pinaforePointLens) $
    MkCloseUnlift identityUnlift literalInverseFunction

literalPinaforeTableLensMorphism :: AsLiteral t => PinaforeLensMorphism PinaforeEntityEdit Point (Maybe t)
literalPinaforeTableLensMorphism =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism literalPinaforeMap $ fmap opoint literalInverseFunction

literalPinaforeLensMorphism ::
       (HasPinaforeEntityEdit baseedit, AsLiteral t) => PinaforeLensMorphism baseedit Point (Maybe t)
literalPinaforeLensMorphism = mapPinaforeLensMorphismBase pinaforePointLens literalPinaforeTableLensMorphism
