{-# OPTIONS -fno-warn-orphans #-}

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

emptyTupleFunction :: forall edita. PureEditFunction edita (TupleEdit EmptyWitness)
emptyTupleFunction = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet :: () -> TupleEditReader EmptyWitness t -> Readable (EditReader edita) t
    editGet () (MkTupleEditReader sel _) = never sel
    editUpdate :: edita -> () -> Readable (EditReader edita) ((), [TupleEdit EmptyWitness])
    editUpdate _ _ = return $ pure []
    in MkEditFunction {..}

emptyTupleLens :: forall edita. PureEditLens edita (TupleEdit EmptyWitness)
emptyTupleLens = let
    editLensFunction :: PureEditFunction edita (TupleEdit EmptyWitness)
    editLensFunction = emptyTupleFunction
    editLensPutEdit :: () -> TupleEdit EmptyWitness -> Readable (EditReader edita) (Maybe ((), [edita]))
    editLensPutEdit () (MkTupleEdit sel _) = never sel
    in MkEditLens {..}

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

firstEditLens :: forall sel edit1. PureEditLens (TupleEdit (ConsWitness edit1 sel)) edit1
firstEditLens = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet :: () -> ReadFunction (TupleEditReader (ConsWitness edit1 sel)) (EditReader edit1)
    editGet () rt = readable $ MkTupleEditReader FirstWitness rt
    editUpdate ::
           TupleEdit (ConsWitness edit1 sel) -> () -> Readable (TupleEditReader (ConsWitness edit1 sel)) ((), [edit1])
    editUpdate (MkTupleEdit FirstWitness edit) () = return ((), [edit])
    editUpdate (MkTupleEdit (RestWitness _) _) () = return ((), [])
    editLensFunction = MkEditFunction {..}
    editLensPutEdit ::
           ()
        -> edit1
        -> Readable (TupleEditReader (ConsWitness edit1 sel)) (Maybe ((), [TupleEdit (ConsWitness edit1 sel)]))
    editLensPutEdit () edit = return $ pure ((), [MkTupleEdit FirstWitness edit])
    in MkEditLens {..}

restEditLens :: forall sel edit1. PureEditLens (TupleEdit (ConsWitness edit1 sel)) (TupleEdit sel)
restEditLens = let
    editAccess :: IOStateAccess ()
    editAccess = unitStateAccess
    editGet :: () -> ReadFunction (TupleEditReader (ConsWitness edit1 sel)) (TupleEditReader sel)
    editGet () (MkTupleEditReader sel rt) = readable $ MkTupleEditReader (RestWitness sel) rt
    editUpdate ::
           TupleEdit (ConsWitness edit1 sel)
        -> ()
        -> Readable (TupleEditReader (ConsWitness edit1 sel)) ((), [TupleEdit sel])
    editUpdate (MkTupleEdit FirstWitness _) () = return ((), [])
    editUpdate (MkTupleEdit (RestWitness sel) edit) () = return ((), [MkTupleEdit sel edit])
    editLensFunction = MkEditFunction {..}
    editLensPutEdit ::
           ()
        -> TupleEdit sel
        -> Readable (TupleEditReader (ConsWitness edit1 sel)) (Maybe ((), [TupleEdit (ConsWitness edit1 sel)]))
    editLensPutEdit () (MkTupleEdit sel edit) = return $ pure ((), [MkTupleEdit (RestWitness sel) edit])
    in MkEditLens {..}

consTuple :: EditSubject a -> Tuple r -> Tuple (ConsWitness a r)
consTuple a (MkTuple tup) =
    MkTuple $ \esel ->
        case esel of
            FirstWitness -> a
            RestWitness sel -> tup sel

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
