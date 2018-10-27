module Pinafore.PredicateMorphism
    ( HasPinaforeEntityEdit(..)
    , predicatePinaforeLensMorphism
    , literalPinaforeInverseFunctionMorphism
    , literalPinaforeLensMorphism
    ) where

import Pinafore.Entity
import Pinafore.Know
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Point
import Shapes
import Truth.Core

class HasPinaforeEntityEdit baseedit where
    pinaforePointLens :: EditLens baseedit PinaforeEntityEdit

instance HasPinaforeEntityEdit PinaforeEntityEdit where
    pinaforePointLens = id

predicatePinaforeMap ::
       Predicate
    -> AnEditLens IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))) (WholeEdit (Know Point))
predicatePinaforeMap prd = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point))) (WholeReader (Know Point))
    efGet mr ReadWhole =
        lift $ do
            ksubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            for ksubj $ \subj -> mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadGetPredicate prd subj
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point)))
        -> IdentityT m [WholeEdit (Know Point)]
    efUpdate (MkTupleEdit SelectContext (PinaforeEntityEditSetPredicate p s v)) mr
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
                val <- mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadGetPredicate prd subj
                return [MkWholeEdit $ Known val]
            Unknown -> return [MkWholeEdit Unknown]
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))) (WholeEdit (Know Point))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Know Point)
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))])
    elPutEdit (MkWholeEdit kv) mr = do
        ksubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        return $
            case ksubj of
                Known subj -> Just [MkTupleEdit SelectContext $ PinaforeEntityEditSetPredicate prd subj kv]
                Unknown ->
                    case kv of
                        Known _ -> Nothing
                        Unknown -> Just []
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know Point)]
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))])
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
    => AnEditLens IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))) (WholeEdit (Know t))
literalPinaforeMap = let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point))) (WholeReader (Know t))
    efGet mr ReadWhole =
        lift $ do
            ksubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            kklit <- for ksubj $ \subj -> mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadToLiteral subj
            return $ do
                klit <- kklit
                lit <- klit
                fromLiteral lit
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point)))
        -> IdentityT m [WholeEdit (Know t)]
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit ksubj)) mr =
        lift $ do
            kklit <- for ksubj $ \subj -> mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadToLiteral subj
            return $
                pure $
                MkWholeEdit $ do
                    klit <- kklit
                    lit <- klit
                    fromLiteral lit
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))) (WholeEdit (Know t))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Know t)
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))])
    elPutEdit (MkWholeEdit kt) mr =
        lift $ do
            kp <- for kt $ \t -> mr $ MkTupleEditReader SelectContext $ PinaforeEntityReadFromLiteral t
            return $ Just [MkTupleEdit SelectContent $ MkWholeEdit kp]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Know t)]
        -> MutableRead m (ContextEditReader PinaforeEntityEdit (WholeEdit (Know Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeEntityEdit (WholeEdit (Know Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

literalInverseFunction ::
       forall t. AsLiteral t
    => APinaforeFunctionMorphism PinaforeEntityEdit IdentityT t Point
literalInverseFunction = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeEntityRead
        -> t
        -> IdentityT m Point
    pfFuncRead mr val = lift $ mr $ PinaforeEntityReadFromLiteral val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeEntityEdit
        -> MutableRead m PinaforeEntityRead
        -> IdentityT m Bool
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

literalPinaforeInverseFunctionMorphism ::
       (HasPinaforeEntityEdit baseedit, AsLiteral t) => PinaforeFunctionMorphism baseedit t Point
literalPinaforeInverseFunctionMorphism =
    mapPinaforeFunctionMorphismBase (editLensFunction pinaforePointLens) $
    MkCloseUnlift identityUnlift literalInverseFunction

literalPinaforeTableLensMorphism :: AsLiteral t => PinaforeLensMorphism PinaforeEntityEdit Point t
literalPinaforeTableLensMorphism =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism literalPinaforeMap $ fmap opoint literalInverseFunction

literalPinaforeLensMorphism :: (HasPinaforeEntityEdit baseedit, AsLiteral t) => PinaforeLensMorphism baseedit Point t
literalPinaforeLensMorphism = mapPinaforeLensMorphismBase pinaforePointLens literalPinaforeTableLensMorphism
