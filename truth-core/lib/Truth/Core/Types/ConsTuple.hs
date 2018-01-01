{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-redundant-constraints #-}

module Truth.Core.Types.ConsTuple where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

instance TupleWitness c EmptyWitness where
    tupleWitness _ = never

instance SubjectTupleSelector EmptyWitness

instance TupleReaderWitness c EmptyWitness where
    tupleReaderWitness _ = never

instance FiniteTupleSelector EmptyWitness where
    tupleConstruct _ = pure $ MkTuple never

emptyTuple :: Tuple EmptyWitness
emptyTuple = MkTuple never

emptyTupleLens :: forall edita. EditLens' edita (TupleEdit EmptyWitness)
emptyTupleLens = let
    efGet :: ReadFunctionT IdentityT (EditReader edita) (TupleEditReader EmptyWitness)
    efGet _ (MkTupleEditReader sel _) = never sel
    efUpdate ::
           forall m. MonadIO m
        => edita
        -> MutableRead m (EditReader edita)
        -> IdentityT m [TupleEdit EmptyWitness]
    efUpdate _ _ = return []
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => TupleEdit EmptyWitness
        -> MutableRead m (EditReader edita)
        -> IdentityT m (Maybe [edita])
    elPutEdit (MkTupleEdit sel _) _ = never sel
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

instance (c a, TupleWitness c r) => TupleWitness c (ConsWitness a r) where
    tupleWitness _ FirstWitness = Dict
    tupleWitness pc (RestWitness r) = tupleWitness pc r

instance (SubjectReader (EditReader a), TestEquality r, TupleReaderWitness SubjectReader r) =>
         SubjectTupleSelector (ConsWitness a r)

instance (c (EditReader a), TupleReaderWitness c r) => TupleReaderWitness c (ConsWitness a r) where
    tupleReaderWitness _ FirstWitness = Dict
    tupleReaderWitness pc (RestWitness r) = tupleReaderWitness pc r

instance (FiniteTupleSelector r, TupleSubject r ~ Tuple r) => FiniteTupleSelector (ConsWitness a r) where
    tupleConstruct getsel =
        (\f (MkTuple r) ->
             MkTuple $ \sel ->
                 case sel of
                     FirstWitness -> f
                     RestWitness rt -> r rt) <$>
        getsel FirstWitness <*>
        tupleConstruct (getsel . RestWitness)

firstEditLens :: forall sel edit1. EditLens' (TupleEdit (ConsWitness edit1 sel)) edit1
firstEditLens = let
    efGet :: ReadFunctionT IdentityT (TupleEditReader (ConsWitness edit1 sel)) (EditReader edit1)
    efGet mr rt = lift $ mr $ MkTupleEditReader FirstWitness rt
    efUpdate ::
           forall m. MonadIO m
        => TupleEdit (ConsWitness edit1 sel)
        -> MutableRead m (TupleEditReader (ConsWitness edit1 sel))
        -> IdentityT m [edit1]
    efUpdate (MkTupleEdit FirstWitness edit) _ = return [edit]
    efUpdate (MkTupleEdit (RestWitness _) _) _ = return []
    elFunction :: AnEditFunction IdentityT (TupleEdit (ConsWitness edit1 sel)) edit1
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => edit1
        -> MutableRead m (EditReader (TupleEdit (ConsWitness edit1 sel)))
        -> IdentityT m (Maybe [TupleEdit (ConsWitness edit1 sel)])
    elPutEdit edit _ = return $ Just [MkTupleEdit FirstWitness edit]
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

restEditLens :: forall sel edit1. EditLens' (TupleEdit (ConsWitness edit1 sel)) (TupleEdit sel)
restEditLens = let
    efGet :: ReadFunctionT IdentityT (TupleEditReader (ConsWitness edit1 sel)) (TupleEditReader sel)
    efGet mr (MkTupleEditReader sel rt) = lift $ mr $ MkTupleEditReader (RestWitness sel) rt
    efUpdate ::
           forall m. MonadIO m
        => TupleEdit (ConsWitness edit1 sel)
        -> MutableRead m (EditReader (TupleEdit (ConsWitness edit1 sel)))
        -> IdentityT m [TupleEdit sel]
    efUpdate (MkTupleEdit FirstWitness _) _ = return []
    efUpdate (MkTupleEdit (RestWitness sel) edit) _ = return [MkTupleEdit sel edit]
    elFunction :: AnEditFunction IdentityT (TupleEdit (ConsWitness edit1 sel)) (TupleEdit sel)
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => TupleEdit sel
        -> MutableRead m (EditReader (TupleEdit (ConsWitness edit1 sel)))
        -> IdentityT m (Maybe [TupleEdit (ConsWitness edit1 sel)])
    elPutEdit (MkTupleEdit sel edit) _ = return $ Just [MkTupleEdit (RestWitness sel) edit]
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

consTuple :: EditSubject a -> Tuple r -> Tuple (ConsWitness a r)
consTuple a (MkTuple tup) =
    MkTuple $ \esel ->
        case esel of
            FirstWitness -> a
            RestWitness sel -> tup sel
{-
consTupleEditFunction ::
       forall s1 s2 edita editb sel.
       EditFunction s1 edita editb
    -> EditFunction s2 edita (TupleEdit sel)
    -> EditFunction (s1, s2) edita (TupleEdit (ConsWitness editb sel))
consTupleEditFunction f1 fr =
    MkEditFunction
    { editAccess = pairStateAccess (editAccess f1) (editAccess fr)
    , editGet =
          \(cur1, curr) ->
              \case
                  MkTupleEditReader FirstWitness rt -> editGet f1 cur1 rt
                  MkTupleEditReader (RestWitness sr) rt -> editGet fr curr $ MkTupleEditReader sr rt
    , editUpdate =
          \ea (old1, oldr) -> do
              (new1, edits1) <- editUpdate f1 ea old1
              (newr, editsr) <- editUpdate fr ea oldr
              return $
                  ( (new1, newr)
                  , fmap (MkTupleEdit FirstWitness) edits1 ++
                    fmap (\(MkTupleEdit sel edit) -> (MkTupleEdit (RestWitness sel) edit)) editsr)
    }

consTupleEditLens ::
       forall s1 s2 edita editb sel.
       EditLens s1 edita editb
    -> EditLens s2 edita (TupleEdit sel)
    -> EditLens (s1, s2) edita (TupleEdit (ConsWitness editb sel))
consTupleEditLens lens1 lensr =
    MkEditLens
    { editLensFunction = consTupleEditFunction (editLensFunction lens1) (editLensFunction lensr)
    , editLensPutEdit =
          \(old1, oldr) ->
              \case
                  MkTupleEdit FirstWitness edit -> do
                      fnedits <- editLensPutEdit lens1 old1 edit
                      return $ fmap (\(new1, editas) -> ((new1, oldr), editas)) fnedits
                  MkTupleEdit (RestWitness sr) edit -> do
                      fnedits <- editLensPutEdit lensr oldr $ MkTupleEdit sr edit
                      return $ fmap (\(newr, editas) -> ((old1, newr), editas)) fnedits
    }
-}
