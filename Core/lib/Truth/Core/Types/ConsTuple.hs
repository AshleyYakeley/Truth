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

    instance TupleHasInfo EmptyWitness where
    {
        tupleHasInfo = never;
    };

    emptyTuple :: Tuple EmptyWitness;
    emptyTuple = MkTuple never;

    emptyTupleFunction :: forall c edita. EditFunction c () edita (TupleEdit EmptyWitness);
    emptyTupleFunction = let
    {
        editInitial = ();

        editGet :: () -> TupleEditReader EmptyWitness t -> Readable c (EditReader edita) t;
        editGet () (MkTupleEditReader sel _) = never sel;

        editUpdate :: edita -> () -> Readable c (EditReader edita) ((), [TupleEdit EmptyWitness]);
        editUpdate _ _ = return $ pure [];
    } in MkEditFunction{..};

    emptyTupleLens :: forall c f edita. EditLens' c f () edita (TupleEdit EmptyWitness);
    emptyTupleLens = let
    {
        editLensFunction :: EditFunction c () edita (TupleEdit EmptyWitness);
        editLensFunction = emptyTupleFunction;

        editLensPutEdit :: () -> TupleEdit EmptyWitness -> Readable c (EditReader edita) (f ((), [edita]));
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

    instance (Edit a,HasTypeInfo a,TupleHasInfo r) => TupleHasInfo (ConsWitness a r) where
    {
        tupleHasInfo FirstWitness = typeInfo;
        tupleHasInfo (RestWitness r) = tupleHasInfo r;
    };

    firstEditLens :: forall c m sel edit1. (Applicative m) =>
        EditLens' c m () (TupleEdit (ConsWitness edit1 sel)) edit1;
    firstEditLens = let
    {
        editInitial = ();

        editGet :: () -> ReadFunction c (TupleEditReader (ConsWitness edit1 sel)) (EditReader edit1);
        editGet () rt = readable $ MkTupleEditReader FirstWitness rt;

        editUpdate :: TupleEdit (ConsWitness edit1 sel) -> () -> Readable c (TupleEditReader (ConsWitness edit1 sel)) ((), [edit1]);
        editUpdate (MkTupleEdit FirstWitness edit) () = return ((),[edit]);
        editUpdate (MkTupleEdit (RestWitness _) _) () = return ((),[]);

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> edit1 -> Readable c (TupleEditReader (ConsWitness edit1 sel)) (m ((),[TupleEdit (ConsWitness edit1 sel)]));
        editLensPutEdit () edit = return $ pure ((),[MkTupleEdit FirstWitness edit]);
    } in MkEditLens{..};

    restEditLens ::  forall c m sel edit1. (Applicative m) =>
        EditLens' c m () (TupleEdit (ConsWitness edit1 sel)) (TupleEdit sel);
    restEditLens = let
    {
        editInitial = ();

        editGet :: () -> ReadFunction c (TupleEditReader (ConsWitness edit1 sel)) (TupleEditReader sel);
        editGet () (MkTupleEditReader sel rt) = readable $ MkTupleEditReader (RestWitness sel) rt;

        editUpdate :: TupleEdit (ConsWitness edit1 sel) -> () -> Readable c (TupleEditReader (ConsWitness edit1 sel)) ((), [TupleEdit sel]);
        editUpdate (MkTupleEdit FirstWitness _) () = return ((),[]);
        editUpdate (MkTupleEdit (RestWitness sel) edit) () = return ((),[MkTupleEdit sel edit]);

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> TupleEdit sel -> Readable c (TupleEditReader (ConsWitness edit1 sel)) (m ((),[TupleEdit (ConsWitness edit1 sel)]));
        editLensPutEdit () (MkTupleEdit sel edit) = return $ pure ((),[MkTupleEdit (RestWitness sel) edit]);
    } in MkEditLens{..};

    consTuple :: EditSubject a -> Tuple r -> Tuple (ConsWitness a r);
    consTuple a (MkTuple tup) = MkTuple $ \esel -> case esel of
    {
        FirstWitness -> a;
        RestWitness sel -> tup sel;
    };

    consTupleFunction :: forall c s1 s2 edita editb sel.
        EditFunction c s1 edita editb -> EditFunction c s2 edita (TupleEdit sel) -> EditFunction c (s1,s2) edita (TupleEdit (ConsWitness editb sel));
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

    consTupleLens :: forall c f s1 s2 edita editb sel. Functor f =>
        EditLens' c f s1 edita editb -> EditLens' c f s2 edita (TupleEdit sel) -> EditLens' c f (s1,s2) edita (TupleEdit (ConsWitness editb sel));
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

    consTupleTypeKnowledge :: TypeKnowledge;
    consTupleTypeKnowledge = $(generateTypeKnowledge [d|
        instance TupleWitness c (EmptyWitness :: * -> *);
        instance TupleSelector (EmptyWitness :: * -> *) where
        {
            type TupleSubject (EmptyWitness :: * -> *) = Tuple (EmptyWitness :: * -> *);
        };
        instance TupleReaderWitness c (EmptyWitness :: * -> *);
        instance FiniteTupleSelector (EmptyWitness :: * -> *);
        instance TupleHasInfo (EmptyWitness :: * -> *);

        instance (c a,TupleWitness c r) => TupleWitness c ((ConsWitness :: * -> (* -> *) -> * -> *) a r);
        instance (Edit a,(TestEquality :: (* -> *) -> Constraint) r,TupleWitness Edit r) => TupleSelector ((ConsWitness :: * -> (* -> *) -> * -> *) a r) where
        {
            type TupleSubject ((ConsWitness :: * -> (* -> *) -> * -> *) a r) = Tuple ((ConsWitness :: * -> (* -> *) -> * -> *) a r);
        };
        instance (c (EditReader a),TupleReaderWitness c r) => TupleReaderWitness c ((ConsWitness :: * -> (* -> *) -> * -> *) a r);
        instance (Edit a,FiniteTupleSelector r,TupleSubject r ~ Tuple r) => FiniteTupleSelector ((ConsWitness :: * -> (* -> *) -> * -> *) a r);
        instance (Edit a,(HasTypeInfo :: * -> Constraint) a,TupleHasInfo r) => TupleHasInfo ((ConsWitness :: * -> (* -> *) -> * -> *) a r);
    |]);
}
