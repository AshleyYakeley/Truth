{-# OPTIONS -fno-warn-orphans #-}
module Truth.Core.Types.ConsTuple where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    instance TupleWitness c EmptyWitness where
    {
        tupleWitness _ = never;
    };

    instance TupleSelector EmptyWitness;

    instance TupleReaderWitness c EmptyWitness where
    {
        tupleReaderWitness _ = never;
    };

    instance FiniteTupleSelector EmptyWitness where
    {
        tupleConstruct _ = pure $ MkTuple never;
    };

    emptyTuple :: Tuple EmptyWitness;
    emptyTuple = MkTuple never;

    emptyTupleFunction :: forall edita. EditFunction () edita (TupleEdit EmptyWitness);
    emptyTupleFunction = let
    {
        editInitial = ();

        editGet :: () -> TupleEditReader EmptyWitness t -> Readable (EditReader edita) t;
        editGet () (MkTupleEditReader sel _) = never sel;

        editUpdate :: edita -> () -> Readable (EditReader edita) ((), [TupleEdit EmptyWitness]);
        editUpdate _ _ = return $ pure [];
    } in MkEditFunction{..};

    emptyTupleLens :: forall edita. EditLens () edita (TupleEdit EmptyWitness);
    emptyTupleLens = let
    {
        editLensFunction :: EditFunction () edita (TupleEdit EmptyWitness);
        editLensFunction = emptyTupleFunction;

        editLensPutEdit :: () -> TupleEdit EmptyWitness -> Readable (EditReader edita) (Maybe ((), [edita]));
        editLensPutEdit () (MkTupleEdit sel _) = never sel;
    } in MkEditLens{..};


    instance (c a,TupleWitness c r) => TupleWitness c (ConsWitness a r) where
    {
        tupleWitness _ FirstWitness = MkConstraintWitness;
        tupleWitness pc (RestWitness r) = tupleWitness pc r;
    };

    instance (Edit a,TestEquality r,TupleWitness Edit r) => TupleSelector (ConsWitness a r);

    instance (c (EditReader a),TupleReaderWitness c r) => TupleReaderWitness c (ConsWitness a r) where
    {
        tupleReaderWitness _ FirstWitness = MkConstraintWitness;
        tupleReaderWitness pc (RestWitness r) = tupleReaderWitness pc r;
    };

    instance (Edit a,FiniteTupleSelector r,TupleSubject r ~ Tuple r) => FiniteTupleSelector (ConsWitness a r) where
    {
        tupleConstruct getsel = (\f (MkTuple r) -> MkTuple $ \sel -> case sel of
        {
            FirstWitness -> f;
            RestWitness rt -> r rt;
        }) <$> getsel FirstWitness <*> tupleConstruct (getsel . RestWitness);
    };

    firstEditLens :: forall sel edit1.
        EditLens () (TupleEdit (ConsWitness edit1 sel)) edit1;
    firstEditLens = let
    {
        editInitial = ();

        editGet :: () -> ReadFunction (TupleEditReader (ConsWitness edit1 sel)) (EditReader edit1);
        editGet () rt = readable $ MkTupleEditReader FirstWitness rt;

        editUpdate :: TupleEdit (ConsWitness edit1 sel) -> () -> Readable (TupleEditReader (ConsWitness edit1 sel)) ((), [edit1]);
        editUpdate (MkTupleEdit FirstWitness edit) () = return ((),[edit]);
        editUpdate (MkTupleEdit (RestWitness _) _) () = return ((),[]);

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> edit1 -> Readable (TupleEditReader (ConsWitness edit1 sel)) (Maybe ((),[TupleEdit (ConsWitness edit1 sel)]));
        editLensPutEdit () edit = return $ pure ((),[MkTupleEdit FirstWitness edit]);
    } in MkEditLens{..};

    restEditLens ::  forall sel edit1.
        EditLens () (TupleEdit (ConsWitness edit1 sel)) (TupleEdit sel);
    restEditLens = let
    {
        editInitial = ();

        editGet :: () -> ReadFunction (TupleEditReader (ConsWitness edit1 sel)) (TupleEditReader sel);
        editGet () (MkTupleEditReader sel rt) = readable $ MkTupleEditReader (RestWitness sel) rt;

        editUpdate :: TupleEdit (ConsWitness edit1 sel) -> () -> Readable (TupleEditReader (ConsWitness edit1 sel)) ((), [TupleEdit sel]);
        editUpdate (MkTupleEdit FirstWitness _) () = return ((),[]);
        editUpdate (MkTupleEdit (RestWitness sel) edit) () = return ((),[MkTupleEdit sel edit]);

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> TupleEdit sel -> Readable (TupleEditReader (ConsWitness edit1 sel)) (Maybe ((),[TupleEdit (ConsWitness edit1 sel)]));
        editLensPutEdit () (MkTupleEdit sel edit) = return $ pure ((),[MkTupleEdit (RestWitness sel) edit]);
    } in MkEditLens{..};

    consTuple :: EditSubject a -> Tuple r -> Tuple (ConsWitness a r);
    consTuple a (MkTuple tup) = MkTuple $ \esel -> case esel of
    {
        FirstWitness -> a;
        RestWitness sel -> tup sel;
    };

    consTupleFunction :: forall s1 s2 edita editb sel.
        EditFunction s1 edita editb -> EditFunction s2 edita (TupleEdit sel) -> EditFunction (s1,s2) edita (TupleEdit (ConsWitness editb sel));
    consTupleFunction f1 fr = MkEditFunction
    {
        editInitial = (editInitial f1,editInitial fr),
        editGet = \(cur1,curr) -> \case
        {
            MkTupleEditReader FirstWitness rt -> editGet f1 cur1 rt;
            MkTupleEditReader (RestWitness sr) rt -> editGet fr curr $ MkTupleEditReader sr rt;
        },
        editUpdate = \ea (old1,oldr) -> do
        {
            (new1,edits1) <- editUpdate f1 ea old1;
            (newr,editsr) <- editUpdate fr ea oldr;
            return $ ((new1,newr),fmap (MkTupleEdit FirstWitness) edits1 ++ fmap (\(MkTupleEdit sel edit) -> (MkTupleEdit (RestWitness sel) edit)) editsr);
        }
    };

    consTupleLens :: forall s1 s2 edita editb sel.
        EditLens s1 edita editb -> EditLens s2 edita (TupleEdit sel) -> EditLens (s1,s2) edita (TupleEdit (ConsWitness editb sel));
    consTupleLens lens1 lensr = MkEditLens
    {
        editLensFunction = consTupleFunction (editLensFunction lens1) (editLensFunction lensr),
        editLensPutEdit = \(old1,oldr) -> \case
        {
            MkTupleEdit FirstWitness edit -> do
            {
                fnedits <- editLensPutEdit lens1 old1 edit;
                return $ fmap (\(new1,editas) -> ((new1,oldr),editas)) fnedits;
            };
            MkTupleEdit (RestWitness sr) edit -> do
            {
                fnedits <- editLensPutEdit lensr oldr $ MkTupleEdit sr edit;
                return $ fmap (\(newr,editas) -> ((old1,newr),editas)) fnedits;
            };
        }
    };
}
