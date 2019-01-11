{-# OPTIONS -fno-warn-orphans #-}

module Truth.Core.Types.ConsTuple where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

instance TupleWitness c EmptyType where
    tupleWitness = never

instance SubjectTupleSelector EmptyType

instance TupleReaderWitness c EmptyType where
    tupleReaderWitness = never

instance FiniteTupleSelector EmptyType where
    tupleConstruct _ = pure $ MkTuple never

emptyTuple :: Tuple EmptyType
emptyTuple = MkTuple never

emptyTupleLens :: forall edita. EditLens edita (TupleEdit EmptyType)
emptyTupleLens = let
    efGet :: ReadFunctionT IdentityT (EditReader edita) (TupleEditReader EmptyType)
    efGet _ (MkTupleEditReader sel _) = never sel
    efUpdate ::
           forall m. MonadIO m
        => edita
        -> MutableRead m (EditReader edita)
        -> IdentityT m [TupleEdit EmptyType]
    efUpdate _ _ = return []
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [TupleEdit EmptyType]
        -> MutableRead m (EditReader edita)
        -> IdentityT m (Maybe [edita])
    elPutEdits [] _ = return $ Just []
    elPutEdits ((MkTupleEdit sel _):_) _ = never sel
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

instance (c a, TupleWitness c r) => TupleWitness c (ConsType a r) where
    tupleWitness FirstType = Dict
    tupleWitness (RestType r) = tupleWitness r

instance (SubjectReader (EditReader a), TestEquality r, TupleReaderWitness SubjectReader r) =>
             SubjectTupleSelector (ConsType a r)

instance (c (EditReader a), TupleReaderWitness c r) => TupleReaderWitness c (ConsType a r) where
    tupleReaderWitness FirstType = Dict
    tupleReaderWitness (RestType r) = tupleReaderWitness r

instance (FiniteTupleSelector r, TupleSubject r ~ Tuple r) => FiniteTupleSelector (ConsType a r) where
    tupleConstruct getsel =
        (\f (MkTuple r) ->
             MkTuple $ \sel ->
                 case sel of
                     FirstType -> f
                     RestType rt -> r rt) <$>
        getsel FirstType <*>
        tupleConstruct (getsel . RestType)

firstEditLens :: forall sel edit1. EditLens (TupleEdit (ConsType edit1 sel)) edit1
firstEditLens = let
    efGet :: ReadFunctionT IdentityT (TupleEditReader (ConsType edit1 sel)) (EditReader edit1)
    efGet mr rt = lift $ mr $ MkTupleEditReader FirstType rt
    efUpdate ::
           forall m. MonadIO m
        => TupleEdit (ConsType edit1 sel)
        -> MutableRead m (TupleEditReader (ConsType edit1 sel))
        -> IdentityT m [edit1]
    efUpdate (MkTupleEdit FirstType edit) _ = return [edit]
    efUpdate (MkTupleEdit (RestType _) _) _ = return []
    elFunction :: AnEditFunction IdentityT (TupleEdit (ConsType edit1 sel)) edit1
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [edit1]
        -> MutableRead m (EditReader (TupleEdit (ConsType edit1 sel)))
        -> IdentityT m (Maybe [TupleEdit (ConsType edit1 sel)])
    elPutEdits edits _ = return $ Just $ fmap (MkTupleEdit FirstType) edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

restEditLens :: forall sel edit1. EditLens (TupleEdit (ConsType edit1 sel)) (TupleEdit sel)
restEditLens = let
    efGet :: ReadFunctionT IdentityT (TupleEditReader (ConsType edit1 sel)) (TupleEditReader sel)
    efGet mr (MkTupleEditReader sel rt) = lift $ mr $ MkTupleEditReader (RestType sel) rt
    efUpdate ::
           forall m. MonadIO m
        => TupleEdit (ConsType edit1 sel)
        -> MutableRead m (EditReader (TupleEdit (ConsType edit1 sel)))
        -> IdentityT m [TupleEdit sel]
    efUpdate (MkTupleEdit FirstType _) _ = return []
    efUpdate (MkTupleEdit (RestType sel) edit) _ = return [MkTupleEdit sel edit]
    elFunction :: AnEditFunction IdentityT (TupleEdit (ConsType edit1 sel)) (TupleEdit sel)
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [TupleEdit sel]
        -> MutableRead m (EditReader (TupleEdit (ConsType edit1 sel)))
        -> IdentityT m (Maybe [TupleEdit (ConsType edit1 sel)])
    elPutEdits edits _ = return $ Just $ fmap (\(MkTupleEdit sel edit) -> MkTupleEdit (RestType sel) edit) edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

consTuple :: EditSubject a -> Tuple r -> Tuple (ConsType a r)
consTuple a (MkTuple tup) =
    MkTuple $ \esel ->
        case esel of
            FirstType -> a
            RestType sel -> tup sel
{-
consTupleEditFunction ::
       forall s1 s2 edita editb sel.
       EditFunction s1 edita editb
    -> EditFunction s2 edita (TupleEdit sel)
    -> EditFunction (s1, s2) edita (TupleEdit (ConsType editb sel))
consTupleEditFunction f1 fr =
    MkEditFunction
    { editAccess = pairStateAccess (editAccess f1) (editAccess fr)
    , editGet =
          \(cur1, curr) ->
              \case
                  MkTupleEditReader FirstType rt -> editGet f1 cur1 rt
                  MkTupleEditReader (RestType sr) rt -> editGet fr curr $ MkTupleEditReader sr rt
    , editUpdate =
          \ea (old1, oldr) -> do
              (new1, edits1) <- editUpdate f1 ea old1
              (newr, editsr) <- editUpdate fr ea oldr
              return $
                  ( (new1, newr)
                  , fmap (MkTupleEdit FirstType) edits1 ++
                    fmap (\(MkTupleEdit sel edit) -> (MkTupleEdit (RestType sel) edit)) editsr)
    }

consTupleEditLens ::
       forall s1 s2 edita editb sel.
       EditLens s1 edita editb
    -> EditLens s2 edita (TupleEdit sel)
    -> EditLens (s1, s2) edita (TupleEdit (ConsType editb sel))
consTupleEditLens lens1 lensr =
    MkEditLens
    { editLensFunction = consTupleEditFunction (editLensFunction lens1) (editLensFunction lensr)
    , editLensPutEdit =
          \(old1, oldr) ->
              \case
                  MkTupleEdit FirstType edit -> do
                      fnedits <- editLensPutEdit lens1 old1 edit
                      return $ fmap (\(new1, editas) -> ((new1, oldr), editas)) fnedits
                  MkTupleEdit (RestType sr) edit -> do
                      fnedits <- editLensPutEdit lensr oldr $ MkTupleEdit sr edit
                      return $ fmap (\(newr, editas) -> ((old1, newr), editas)) fnedits
    }
-}
