module Pinafore.PredicateMorphism
    ( HasPinaforePointEdit(..)
    , predicatePinaforeLensMorphism
    , literalPinaforeLensMorphism
    ) where

import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Point
import Pinafore.Table (Predicate)
import Shapes
import Truth.Core

class HasPinaforePointEdit baseedit where
    pinaforePointLens :: EditLens baseedit PinaforePointEdit

instance HasPinaforePointEdit PinaforePointEdit where
    pinaforePointLens = id

predicatePinaforeMap ::
       Predicate -> AnEditLens IdentityT (ContextEdit PinaforePointEdit (WholeEdit Point)) (WholeEdit Point)
predicatePinaforeMap prd = let
    efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforePointEdit (WholeEdit Point)) (WholeReader Point)
    efGet mr ReadWhole =
        lift $ do
            subj <- mr $ MkTupleEditReader SelectContent ReadWhole
            mr $ MkTupleEditReader SelectContext $ PinaforePointReadGetPredicate prd subj
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforePointEdit (WholeEdit Point)
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit Point))
        -> IdentityT m [WholeEdit Point]
    efUpdate (MkTupleEdit SelectContext (PinaforePointEditSetPredicate p s v)) mr
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
            val <- mr $ MkTupleEditReader SelectContext $ PinaforePointReadGetPredicate prd subj
            return [MkWholeEdit val]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforePointEdit (WholeEdit Point)) (WholeEdit Point)
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit Point
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit Point)])
    elPutEdit (MkWholeEdit v) mr = do
        subj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        return $ Just [MkTupleEdit SelectContext $ PinaforePointEditSetPredicate prd subj v]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit Point]
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit Point)])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

predicateInverseFunction :: Predicate -> APinaforeFunctionMorphism PinaforePointEdit IdentityT Point (FiniteSet Point)
predicateInverseFunction prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforePointRead
        -> Point
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforePointReadLookupPredicate prd val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforePointEdit
        -> MutableRead m PinaforePointRead
        -> IdentityT m Bool
    pfUpdate (PinaforePointEditSetPredicate p _ _) _
        | p == prd = return True
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

predicatePinaforeTableLensMorphism :: Predicate -> PinaforeLensMorphism PinaforePointEdit Point Point
predicatePinaforeTableLensMorphism prd =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism (predicatePinaforeMap prd) (predicateInverseFunction prd)

predicatePinaforeLensMorphism :: HasPinaforePointEdit baseedit => Predicate -> PinaforeLensMorphism baseedit Point Point
predicatePinaforeLensMorphism prd =
    mapPinaforeLensMorphismBase pinaforePointLens $ predicatePinaforeTableLensMorphism prd

literalPinaforeMap ::
       forall t. AsLiteral t
    => AnEditLens IdentityT (ContextEdit PinaforePointEdit (WholeEdit Point)) (WholeEdit (Maybe t))
literalPinaforeMap = let
    efGet :: ReadFunctionT IdentityT (ContextEditReader PinaforePointEdit (WholeEdit Point)) (WholeReader (Maybe t))
    efGet mr ReadWhole =
        lift $ do
            subj <- mr $ MkTupleEditReader SelectContent ReadWhole
            mlit <- mr $ MkTupleEditReader SelectContext $ PinaforePointReadToLiteral subj
            return $ mlit >>= fromLiteral
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforePointEdit (WholeEdit Point)
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit Point))
        -> IdentityT m [WholeEdit (Maybe t)]
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit subj)) mr =
        lift $ do
            mlit <- mr $ MkTupleEditReader SelectContext $ PinaforePointReadToLiteral subj
            return [MkWholeEdit $ mlit >>= fromLiteral]
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforePointEdit (WholeEdit Point)) (WholeEdit (Maybe t))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe t)
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit Point)])
    elPutEdit (MkWholeEdit (Just t)) mr =
        lift $ do
            p <- mr $ MkTupleEditReader SelectContext $ PinaforePointReadFromLiteral t
            return $ Just [MkTupleEdit SelectContent $ MkWholeEdit p]
    elPutEdit (MkWholeEdit Nothing) _ = do
        e <- newPoint
        return $ Just [MkTupleEdit SelectContent $ MkWholeEdit e]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe t)]
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit Point))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit Point)])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

literalInverseFunction ::
       forall t. AsLiteral t
    => APinaforeFunctionMorphism PinaforePointEdit IdentityT (Maybe t) (FiniteSet Point)
literalInverseFunction = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforePointRead
        -> Maybe t
        -> IdentityT m (FiniteSet Point)
    pfFuncRead _ Nothing = do
        e <- newPoint
        return $ opoint e
    pfFuncRead mr (Just val) =
        lift $ do
            p <- mr $ PinaforePointReadFromLiteral val
            return $ opoint p
    pfUpdate ::
           forall m. MonadIO m
        => PinaforePointEdit
        -> MutableRead m PinaforePointRead
        -> IdentityT m Bool
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

literalPinaforeTableLensMorphism :: AsLiteral t => PinaforeLensMorphism PinaforePointEdit Point (Maybe t)
literalPinaforeTableLensMorphism =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism literalPinaforeMap literalInverseFunction

literalPinaforeLensMorphism ::
       (HasPinaforePointEdit baseedit, AsLiteral t) => PinaforeLensMorphism baseedit Point (Maybe t)
literalPinaforeLensMorphism = mapPinaforeLensMorphismBase pinaforePointLens literalPinaforeTableLensMorphism
