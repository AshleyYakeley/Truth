module Truth.Core.Types.Pair where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


    data PairSelector ea eb (et :: *) where
    {
        EditFirst :: PairSelector ea eb ea;
        EditSecond :: PairSelector ea eb eb;
    };

    type PairEditReader ea eb = TupleEditReader (PairSelector ea eb);
    type PairEdit ea eb = TupleEdit (PairSelector ea eb);

    firstReadFunction :: PureReadFunction (PairEditReader ea eb) (EditReader ea);
    firstReadFunction = tupleReadFunction EditFirst;

    secondReadFunction :: PureReadFunction (PairEditReader ea eb) (EditReader eb);
    secondReadFunction = tupleReadFunction EditSecond;

    instance TestEquality (PairSelector ea eb) where
    {
        testEquality EditFirst EditFirst = Just Refl;
        testEquality EditSecond EditSecond = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (Edit ea,Edit eb) => TupleSelector (PairSelector ea eb) where
    {
        type TupleSubject (PairSelector ea eb) = (EditSubject ea,EditSubject eb);
        tupleReadFrom EditFirst (a,_b) = a;
        tupleReadFrom EditSecond (_a,b) = b;
    };

    instance (Edit ea,Edit eb) => FiniteTupleSelector (PairSelector ea eb) where
    {
        tupleConstruct f = (,) <$> f EditFirst <*> f EditSecond;
    };

    instance (c (EditReader ea),c (EditReader eb)) => TupleReaderWitness c (PairSelector ea eb) where
    {
        tupleReaderWitness _ EditFirst = MkConstraintWitness;
        tupleReaderWitness _ EditSecond = MkConstraintWitness;
    };

    instance (c ea,c eb) => TupleWitness c (PairSelector ea eb) where
    {
        tupleWitness _ EditFirst = MkConstraintWitness;
        tupleWitness _ EditSecond = MkConstraintWitness;
    };

    instance (Edit ea,HasTypeInfo ea,Edit eb,HasTypeInfo eb) =>
        TupleHasInfo (PairSelector ea eb) where
    {
        tupleHasInfo EditFirst = typeInfo;
        tupleHasInfo EditSecond = typeInfo;
    };

    $(return []);
    instance HasTypeInfo PairSelector where
    {
        typeWitness = $(generateWitness [t|PairSelector|]);
        typeName _ = "PairSelector";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            --instance TestEquality (PairSelector ea eb);
            instance (Edit ea,Edit eb) =>
                TupleSelector (PairSelector ea eb) where
            {
                type TupleSubject (PairSelector ea eb) = (EditSubject ea,EditSubject eb);
            };
            instance (Edit ea,Edit eb) =>
                FiniteTupleSelector (PairSelector ea eb);
            instance (c (EditReader ea),c (EditReader eb)) => TupleReaderWitness c (PairSelector ea eb);
            instance (c ea,c eb) => TupleWitness c (PairSelector ea eb);
            instance (Edit ea,(HasTypeInfo :: * -> Constraint) ea,Edit eb,(HasTypeInfo :: * -> Constraint) eb) =>
                TupleHasInfo (PairSelector ea eb);
        |]);
    };

    partitionPairEdits :: forall ea eb. [PairEdit ea eb] -> ([ea], [eb]);
    partitionPairEdits pes = let
    {
        toEither :: PairEdit ea eb -> Either ea eb;
        toEither (MkTupleEdit EditFirst ea) = Left ea;
        toEither (MkTupleEdit EditSecond eb) = Right eb;
    } in partitionEithers $ fmap toEither pes;

    pairMutableRead :: MutableRead m (EditReader ea) -> MutableRead m (EditReader eb) -> MutableRead m (PairEditReader ea eb);
    pairMutableRead mra _mrb (MkTupleEditReader EditFirst ra) = mra ra;
    pairMutableRead _mra mrb (MkTupleEditReader EditSecond rb) = mrb rb;

    fstLiftEditLens :: forall c f state editx edita editb. (ReadableConstraint c,Applicative f) =>
        EditLens' c f state edita editb -> EditLens' c f state (PairEdit edita editx) (PairEdit editb editx);
    fstLiftEditLens (MkEditLens (MkEditFunction editInitial g u) pe) = let
    {
        editGet :: state -> ReadFunction c (PairEditReader edita editx) (PairEditReader editb editx);
        editGet _ (MkTupleEditReader EditSecond rt) = readable (MkTupleEditReader EditSecond rt);
        editGet curstate (MkTupleEditReader EditFirst rt) = mapReadable firstReadFunction $ g curstate rt;

        editUpdate :: PairEdit edita editx -> state -> Readable c (PairEditReader edita editx) (state,[PairEdit editb editx]);
        editUpdate (MkTupleEdit EditSecond ex) oldstate = return (oldstate,[MkTupleEdit EditSecond ex]);
        editUpdate (MkTupleEdit EditFirst ea) oldstate = mapReadable firstReadFunction $ do
        {
            (newstate,ebs) <- u ea oldstate;
            return (newstate,fmap (MkTupleEdit EditFirst) ebs);
        };

        editLensPutEdit :: state -> PairEdit editb editx -> Readable c (PairEditReader edita editx) (f (state,[PairEdit edita editx]));
        editLensPutEdit oldstate (MkTupleEdit EditSecond ex) = return $ pure $ (oldstate,[MkTupleEdit EditSecond ex]);
        editLensPutEdit oldstate (MkTupleEdit EditFirst eb) = mapReadable firstReadFunction $ do
        {
            msa <- pe oldstate eb;
            return $ fmap (\(newstate,eas) -> (newstate,fmap (MkTupleEdit EditFirst) eas)) msa;
        };

        editLensFunction = MkEditFunction{..};
    } in MkEditLens{..};

    sndLiftEditLens :: forall c f state editx edita editb. (ReadableConstraint c,Applicative f) =>
        EditLens' c f state edita editb -> EditLens' c f state (PairEdit editx edita) (PairEdit editx editb);
    sndLiftEditLens (MkEditLens (MkEditFunction editInitial g u) pe) = let
    {
        editGet :: state -> ReadFunction c (PairEditReader editx edita) (PairEditReader editx editb);
        editGet _ (MkTupleEditReader EditFirst rt) = readable (MkTupleEditReader EditFirst rt);
        editGet curstate (MkTupleEditReader EditSecond rt) = mapReadable secondReadFunction $ g curstate rt;

        editUpdate :: PairEdit editx edita -> state -> Readable c (PairEditReader editx edita) (state,[PairEdit editx editb]);
        editUpdate (MkTupleEdit EditFirst ex) oldstate = return (oldstate,[MkTupleEdit EditFirst ex]);
        editUpdate (MkTupleEdit EditSecond ea) oldstate = mapReadable secondReadFunction $ do
        {
            (newstate,ebs) <- u ea oldstate;
            return (newstate,fmap (MkTupleEdit EditSecond) ebs);
        };

        editLensPutEdit :: state -> PairEdit editx editb -> Readable c (PairEditReader editx edita) (f (state,[PairEdit editx edita]));
        editLensPutEdit oldstate (MkTupleEdit EditFirst ex) = return $ pure $ (oldstate,[MkTupleEdit EditFirst ex]);
        editLensPutEdit oldstate (MkTupleEdit EditSecond eb) = mapReadable secondReadFunction $ do
        {
            msa <- pe oldstate eb;
            return $ fmap (\(newstate,eas) -> (newstate,fmap (MkTupleEdit EditSecond) eas)) msa;
        };

        editLensFunction = MkEditFunction{..};
    } in MkEditLens{..};

    pairJoinEditFunctions :: forall c s1 s2 edita editb1 editb2.
        EditFunction c s1 edita editb1 -> EditFunction c s2 edita editb2 -> EditFunction c (s1,s2) edita (PairEdit editb1 editb2);
    pairJoinEditFunctions ef1 ef2 = MkEditFunction
    {
        editInitial = (editInitial ef1,editInitial ef2),
        editGet = \(cur1,cur2) -> \case
        {
            MkTupleEditReader EditFirst rt -> editGet ef1 cur1 rt;
            MkTupleEditReader EditSecond rt -> editGet ef2 cur2 rt;
        },
        editUpdate = \ea (old1,old2) -> do
        {
            (new1,eb1s) <- editUpdate ef1 ea old1;
            (new2,eb2s) <- editUpdate ef2 ea old2;
            return ((new1,new2),fmap (MkTupleEdit EditFirst) eb1s ++ fmap (MkTupleEdit EditSecond) eb2s);
        }
    };

    pairJoinEditLenses :: forall c f s1 s2 edita editb1 editb2. Functor f =>
        EditLens' c f s1 edita editb1 -> EditLens' c f s2 edita editb2 -> EditLens' c f (s1,s2) edita (PairEdit editb1 editb2);
    pairJoinEditLenses lens1 lens2 = MkEditLens
    {
        editLensFunction = pairJoinEditFunctions (editLensFunction lens1) (editLensFunction lens2),
        editLensPutEdit = \(old1,old2) -> \case
        {
            MkTupleEdit EditFirst editb -> do
            {
                fseas <- editLensPutEdit lens1 old1 editb;
                return $ fmap (\(new1,eas) -> ((new1,old2),eas)) fseas;
            };
            MkTupleEdit EditSecond editb -> do
            {
                fseas <- editLensPutEdit lens2 old2 editb;
                return $ fmap (\(new2,eas) -> ((old1,new2),eas)) fseas;
            };
        }
    };
}
