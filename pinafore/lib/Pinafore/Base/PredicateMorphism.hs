module Pinafore.Base.PredicateMorphism
    ( HasPinaforePointEdit(..)
    , predicatePinaforeLensMorphism
    , literalPinaforeInverseFunctionMorphism
    , literalPinaforeLensMorphism
    ) where

import Pinafore.Base.Edit
import Pinafore.Base.Know
import Pinafore.Base.Literal
import Pinafore.Base.Morphism
import Pinafore.Base.Point
import Shapes
import Truth.Core

class HasPinaforePointEdit baseedit where
    pinaforePointLens :: EditLens baseedit PinaforePointEdit

instance HasPinaforePointEdit PinaforePointEdit where
    pinaforePointLens = id

predicatePinaforeMap ::
       Predicate
    -> AnEditLens IdentityT (ContextEdit PinaforePointEdit (WholeEdit (Know Point))) (WholeEdit (Know Point))
predicatePinaforeMap prd = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforePointEdit (WholeEdit (Know Point))) (WholeReader (Know Point))
    efGet mr ReadWhole =
        lift $ do
            ksubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            for ksubj $ \subj -> mr $ MkTupleEditReader SelectContext $ PinaforePointReadGetPredicate prd subj
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforePointEdit (WholeEdit (Know Point))
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know Point)))
        -> IdentityT m [WholeEdit (Know Point)]
    efUpdate (MkTupleEdit SelectContext (PinaforePointEditSetPredicate p s v)) mr
        | p == prd =
            lift $ do
                ksubj <- mr $ MkTupleEditReader SelectContent ReadWhole
                return $
                    if Known s == ksubj
                        then [MkWholeEdit v]
                        else []
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit ksubj)) mr =
        lift $
        case ksubj of
            Known subj -> do
                val <- mr $ MkTupleEditReader SelectContext $ PinaforePointReadGetPredicate prd subj
                return [MkWholeEdit $ Known val]
            Unknown -> return [MkWholeEdit Unknown]
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforePointEdit (WholeEdit (Know Point))) (WholeEdit (Know Point))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Know Point)
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit (Know Point))])
    elPutEdit (MkWholeEdit kv) mr = do
        ksubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        return $
            case ksubj of
                Known subj -> Just [MkTupleEdit SelectContext $ PinaforePointEditSetPredicate prd subj kv]
                Unknown ->
                    case kv of
                        Known _ -> Nothing
                        Unknown -> Just []
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know Point)]
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit (Know Point))])
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
    => AnEditLens IdentityT (ContextEdit PinaforePointEdit (WholeEdit (Know Point))) (WholeEdit (Know t))
literalPinaforeMap = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforePointEdit (WholeEdit (Know Point))) (WholeReader (Know t))
    efGet mr ReadWhole =
        lift $ do
            ksubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            kklit <- for ksubj $ \subj -> mr $ MkTupleEditReader SelectContext $ PinaforePointReadToLiteral subj
            return $ do
                klit <- kklit
                lit <- klit
                fromLiteral lit
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforePointEdit (WholeEdit (Know Point))
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know Point)))
        -> IdentityT m [WholeEdit (Know t)]
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit ksubj)) mr =
        lift $ do
            kklit <- for ksubj $ \subj -> mr $ MkTupleEditReader SelectContext $ PinaforePointReadToLiteral subj
            return $
                pure $
                MkWholeEdit $ do
                    klit <- kklit
                    lit <- klit
                    fromLiteral lit
    elFunction :: AnEditFunction IdentityT (ContextEdit PinaforePointEdit (WholeEdit (Know Point))) (WholeEdit (Know t))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Know t)
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit (Know Point))])
    elPutEdit (MkWholeEdit kt) mr =
        lift $ do
            kp <- for kt $ \t -> mr $ MkTupleEditReader SelectContext $ PinaforePointReadFromLiteral t
            return $ Just [MkTupleEdit SelectContent $ MkWholeEdit kp]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know t)]
        -> MutableRead m (ContextEditReader PinaforePointEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforePointEdit (WholeEdit (Know Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

literalInverseFunction ::
       forall t. AsLiteral t
    => APinaforeFunctionMorphism PinaforePointEdit IdentityT t Point
literalInverseFunction = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforePointRead
        -> t
        -> IdentityT m Point
    pfFuncRead mr val = lift $ mr $ PinaforePointReadFromLiteral val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforePointEdit
        -> MutableRead m PinaforePointRead
        -> IdentityT m Bool
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

literalPinaforeInverseFunctionMorphism ::
       (HasPinaforePointEdit baseedit, AsLiteral t) => PinaforeFunctionMorphism baseedit t Point
literalPinaforeInverseFunctionMorphism =
    mapPinaforeFunctionMorphismBase (editLensFunction pinaforePointLens) $
    MkCloseUnlift identityUnlift literalInverseFunction

literalPinaforeTableLensMorphism :: AsLiteral t => PinaforeLensMorphism PinaforePointEdit Point t
literalPinaforeTableLensMorphism =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism literalPinaforeMap $ fmap opoint literalInverseFunction

literalPinaforeLensMorphism :: (HasPinaforePointEdit baseedit, AsLiteral t) => PinaforeLensMorphism baseedit Point t
literalPinaforeLensMorphism = mapPinaforeLensMorphismBase pinaforePointLens literalPinaforeTableLensMorphism
