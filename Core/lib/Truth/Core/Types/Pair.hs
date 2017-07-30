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

    firstReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader ea);
    firstReadFunction = tupleReadFunction EditFirst;

    secondReadFunction :: ReadFunction (PairEditReader ea eb) (EditReader eb);
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

    liftFstPairFloatingEditLens :: forall c f state editx edita editb. (ReadableConstraint c,Applicative f) =>
        GenFloatingEditLens' c f state edita editb -> GenFloatingEditLens' c f state (PairEdit edita editx) (PairEdit editb editx);
    liftFstPairFloatingEditLens (MkFloatingEditLens (MkFloatingEditFunction floatingEditInitial g u) pe) = let
    {
        floatingEditGet :: state -> GenReadFunction c (PairEditReader edita editx) (PairEditReader editb editx);
        floatingEditGet _ (MkTupleEditReader EditSecond rt) = readable (MkTupleEditReader EditSecond rt);
        floatingEditGet curstate (MkTupleEditReader EditFirst rt) = mapReadable firstReadFunction $ g curstate rt;

        floatingEditUpdate :: PairEdit edita editx -> state -> GenReadable c (PairEditReader edita editx) (state,[PairEdit editb editx]);
        floatingEditUpdate (MkTupleEdit EditSecond ex) oldstate = return (oldstate,[MkTupleEdit EditSecond ex]);
        floatingEditUpdate (MkTupleEdit EditFirst ea) oldstate = mapReadable firstReadFunction $ do
        {
            (newstate,ebs) <- u ea oldstate;
            return (newstate,fmap (MkTupleEdit EditFirst) ebs);
        };

        floatingEditLensPutEdit :: state -> PairEdit editb editx -> GenReadable c (PairEditReader edita editx) (f (state,[PairEdit edita editx]));
        floatingEditLensPutEdit oldstate (MkTupleEdit EditSecond ex) = return $ pure $ (oldstate,[MkTupleEdit EditSecond ex]);
        floatingEditLensPutEdit oldstate (MkTupleEdit EditFirst eb) = mapReadable firstReadFunction $ do
        {
            msa <- pe oldstate eb;
            return $ fmap (\(newstate,eas) -> (newstate,fmap (MkTupleEdit EditFirst) eas)) msa;
        };

        floatingEditLensFunction = MkFloatingEditFunction{..};
    } in MkFloatingEditLens{..};

    liftSndPairFloatingEditLens :: forall c f state editx edita editb. (ReadableConstraint c,Applicative f) =>
        GenFloatingEditLens' c f state edita editb -> GenFloatingEditLens' c f state (PairEdit editx edita) (PairEdit editx editb);
    liftSndPairFloatingEditLens (MkFloatingEditLens (MkFloatingEditFunction floatingEditInitial g u) pe) = let
    {
        floatingEditGet :: state -> GenReadFunction c (PairEditReader editx edita) (PairEditReader editx editb);
        floatingEditGet _ (MkTupleEditReader EditFirst rt) = readable (MkTupleEditReader EditFirst rt);
        floatingEditGet curstate (MkTupleEditReader EditSecond rt) = mapReadable secondReadFunction $ g curstate rt;

        floatingEditUpdate :: PairEdit editx edita -> state -> GenReadable c (PairEditReader editx edita) (state,[PairEdit editx editb]);
        floatingEditUpdate (MkTupleEdit EditFirst ex) oldstate = return (oldstate,[MkTupleEdit EditFirst ex]);
        floatingEditUpdate (MkTupleEdit EditSecond ea) oldstate = mapReadable secondReadFunction $ do
        {
            (newstate,ebs) <- u ea oldstate;
            return (newstate,fmap (MkTupleEdit EditSecond) ebs);
        };

        floatingEditLensPutEdit :: state -> PairEdit editx editb -> GenReadable c (PairEditReader editx edita) (f (state,[PairEdit editx edita]));
        floatingEditLensPutEdit oldstate (MkTupleEdit EditFirst ex) = return $ pure $ (oldstate,[MkTupleEdit EditFirst ex]);
        floatingEditLensPutEdit oldstate (MkTupleEdit EditSecond eb) = mapReadable secondReadFunction $ do
        {
            msa <- pe oldstate eb;
            return $ fmap (\(newstate,eas) -> (newstate,fmap (MkTupleEdit EditSecond) eas)) msa;
        };

        floatingEditLensFunction = MkFloatingEditFunction{..};
    } in MkFloatingEditLens{..};
}
