module Pinafore.PredicateMorphism
    ( HasPinaforeTableEdit(..)
    , literalPinaforeLensMorphism
    , predicatePinaforeLensMorphism
    ) where

import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Table
import Shapes
import Truth.Core

class HasPinaforeTableEdit baseedit where
    pinaforeTableLens :: EditLens baseedit PinaforeTableEdit

instance HasPinaforeTableEdit PinaforeTableEdit where
    pinaforeTableLens = id

literalPinaforeMap ::
       forall val. AsLiteral val
    => AnEditLens IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe val))
literalPinaforeMap = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeReader (Maybe val))
    efGet mr ReadWhole =
        lift $ do
            msubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            case msubj of
                Just subj -> do
                    mbs <- mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetLiteral subj
                    return $ mbs >>= fromLiteral
                Nothing -> return Nothing
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m [WholeEdit (Maybe val)]
    efUpdate (MkTupleEdit SelectContext (PinaforeTableEditSetLiteral s mbs)) mr = do
        msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        return $
            if Just s == msubj
                then [MkWholeEdit $ mbs >>= fromLiteral]
                else []
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit Nothing)) _ = return [MkWholeEdit Nothing]
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit (Just subj))) mr = do
        mbs <- lift $ mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetLiteral subj
        return $ [MkWholeEdit $ mbs >>= fromLiteral]
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe val))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe val)
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdit (MkWholeEdit (fmap toLiteral -> mbs)) mr = do
        msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        case msubj of
            Just subj -> return $ Just [MkTupleEdit SelectContext $ PinaforeTableEditSetLiteral subj mbs]
            Nothing -> do
                subj <- liftIO randomIO
                return $
                    Just
                        [ MkTupleEdit SelectContent $ MkWholeEdit $ Just subj
                        , MkTupleEdit SelectContext $ PinaforeTableEditSetLiteral subj mbs
                        ]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe val)]
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

literalInverseFunction ::
       forall val. AsLiteral val
    => APinaforeFunctionMorphism PinaforeTableEdit IdentityT val (FiniteSet Point)
literalInverseFunction = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeTableRead
        -> val
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforeTableReadLookupLiteral $ toLiteral val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeTableEdit
        -> MutableRead m PinaforeTableRead
        -> IdentityT m Bool
    pfUpdate (PinaforeTableEditSetLiteral _ _) _ = return True
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

literalPinaforeTableLensMorphism ::
       forall val. AsLiteral val
    => PinaforeLensMorphism PinaforeTableEdit Point val
literalPinaforeTableLensMorphism =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism literalPinaforeMap literalInverseFunction

literalPinaforeLensMorphism :: (HasPinaforeTableEdit baseedit, AsLiteral val) => PinaforeLensMorphism baseedit Point val
literalPinaforeLensMorphism = mapPinaforeLensMorphismBase pinaforeTableLens literalPinaforeTableLensMorphism

predicatePinaforeMap ::
       Predicate
    -> AnEditLens IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe Point))
predicatePinaforeMap prd = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeReader (Maybe Point))
    efGet mr ReadWhole =
        lift $ do
            msubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            case msubj of
                Just subj -> mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetValue prd subj
                Nothing -> return Nothing
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m [WholeEdit (Maybe Point)]
    efUpdate (MkTupleEdit SelectContext (PinaforeTableEditSetValue p s mv)) mr
        | p == prd = do
            msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            return $
                if Just s == msubj
                    then [MkWholeEdit mv]
                    else []
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit Nothing)) _ = return [MkWholeEdit Nothing]
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit (Just subj))) mr = do
        mval <- lift $ mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetValue prd subj
        return [MkWholeEdit mval]
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe Point))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe Point)
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdit (MkWholeEdit mv) mr = do
        msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        case msubj of
            Just subj -> return $ Just [MkTupleEdit SelectContext $ PinaforeTableEditSetValue prd subj mv]
            Nothing -> do
                subj <- liftIO randomIO
                return $
                    Just
                        [ MkTupleEdit SelectContent $ MkWholeEdit $ Just subj
                        , MkTupleEdit SelectContext $ PinaforeTableEditSetValue prd subj mv
                        ]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe Point)]
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

predicateInverseFunction :: Predicate -> APinaforeFunctionMorphism PinaforeTableEdit IdentityT Point (FiniteSet Point)
predicateInverseFunction prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeTableRead
        -> Point
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforeTableReadLookupValue prd val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeTableEdit
        -> MutableRead m PinaforeTableRead
        -> IdentityT m Bool
    pfUpdate (PinaforeTableEditSetValue p _ _) _
        | p == prd = return True
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

predicatePinaforeTableLensMorphism :: Predicate -> PinaforeLensMorphism PinaforeTableEdit Point Point
predicatePinaforeTableLensMorphism prd =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism (predicatePinaforeMap prd) (predicateInverseFunction prd)

predicatePinaforeLensMorphism :: HasPinaforeTableEdit baseedit => Predicate -> PinaforeLensMorphism baseedit Point Point
predicatePinaforeLensMorphism prd =
    mapPinaforeLensMorphismBase pinaforeTableLens $ predicatePinaforeTableLensMorphism prd
