module Truth.Core.Read.ReadFunction where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.FullReader;
    import Truth.Core.Read.Readable;
    import Truth.Core.Read.WriterReadable;


    type GenReadFunction c readera readerb = forall t. readerb t -> GenReadable c readera t;
    type IOReadFunction readera readerb = GenReadFunction MonadIO readera readerb;
    type ReadFunction readera readerb = GenReadFunction Monad readera readerb;

    readFunctionToGen :: ReadFunction readera readerb -> GenReadFunction c readera readerb;
    readFunctionToGen rf rb = readableToGen $ rf rb;

    mapMutableRead :: forall c m ra rb. (Monad m,c m) => GenReadFunction c ra rb -> MutableRead m ra -> MutableRead m rb;
    mapMutableRead rfab sma rbt = unReadable (rfab rbt) sma;

    mapMutableReadW :: forall c m ra rb. (Monad m,c m) => GenReadFunction c ra rb -> MutableReadW m ra -> MutableReadW m rb;
    mapMutableReadW rf (MkMutableReadW mr) = MkMutableReadW $ mapMutableRead rf mr;

    composeReadFunction :: forall c ra rb rc. ReadableConstraint c => GenReadFunction c rb rc -> GenReadFunction c ra rb -> GenReadFunction c ra rc;
    composeReadFunction = case selfReadable @c @ra of
    {
        MkConstraintWitness -> mapMutableRead;
    };

    makeReadFunction :: (Reader rb) => GenReadable c ra (ReaderSubject rb) -> GenReadFunction c ra rb;
    makeReadFunction = readFromM;

    simpleReadFunction :: (GenFullReader c ra,Reader rb) => (ReaderSubject ra -> ReaderSubject rb) -> GenReadFunction c ra rb;
    simpleReadFunction ab = makeReadFunction (fmap ab genFromReader);

    convertReadFunction :: (GenFullReader c ra,Reader rb,ReaderSubject ra ~ ReaderSubject rb) => GenReadFunction c ra rb;
    convertReadFunction = simpleReadFunction id;

    fromReadFunction :: (Reader ra,FullReader rb) => ReadFunction ra rb -> ReaderSubject ra -> ReaderSubject rb;
    fromReadFunction rf = fromReadable (mapReadable rf fromReader);

    mapGenReadable :: forall c ra rb t. ReadableConstraint c => GenReadFunction c ra rb -> GenReadable c rb t -> GenReadable c ra t;
    mapGenReadable rf (MkReadable srbmt) = case selfReadable @c @ra of
    {
        MkConstraintWitness -> srbmt $ \rt -> rf rt;
    };

    fromReadFunctionM :: forall c m ra rb. (Monad m,ReadableConstraint c,c m,Reader ra,GenFullReader c rb) => GenReadFunction c ra rb -> m (ReaderSubject ra) -> m (ReaderSubject rb);
    fromReadFunctionM rf mra = unReadable (mapGenReadable rf genFromReader) $ readFromM mra;

    readFunctionStateT :: (Monad m,FullReader r) => ReadFunction r r -> StateT (ReaderSubject r) m ();
    readFunctionStateT rf = StateT $ \oldval -> return ((),fromReadFunction rf oldval);

    type ReadFunctionF f readera readerb = forall t. readerb t -> Readable readera (f t);

    mapStructureF :: Monad m => ReadFunctionF f ra rb -> MutableRead m ra -> MutableRead (Compose m f) rb;
    mapStructureF rff sa rbt = MkCompose $ unReadable (rff rbt) sa;

    composeReadFunctionF :: ReadFunctionF f rb rc -> ReadFunction ra rb -> ReadFunctionF f ra rc;
    composeReadFunctionF rbc rab rct = mapReadable rab (rbc rct);


    class MapReadable readable where
    {
        mapReadable :: ReadFunction ra rb -> readable rb t -> readable ra t;
        mapReadableF :: (Monad f,Traversable f) => ReadFunctionF f ra rb -> readable rb t -> readable ra (f t);
    };

    instance HasTypeInfo MapReadable where
    {
        typeWitness = $(generateWitness [t|MapReadable|]);
        typeName _ = "MapReadable";
    };

    instance ReadableConstraint c => MapReadable (GenReadable c) where
    {
        mapReadable (rf :: ReadFunction ra rb) (MkReadable srbmt) = case selfReadable @c @ra of
        {
            MkConstraintWitness -> srbmt $ \rt -> readableToGen $ rf rt;
        };
        mapReadableF (rff :: ReadFunctionF f ra rb) (MkReadable srbmt) = case selfComposeReadable @c @f @ra of
        {
            MkConstraintWitness -> getCompose $ srbmt $ \rt -> MkCompose $ readableToGen $ rff rt;
        };
    };


    class ReadableConstraint c where
    {
        selfCompose :: forall f m. (Monad f,Traversable f,c m) => ConstraintWitness (c (Compose m f));
        selfWriterT :: forall w m. (Monad m,Monoid w,c m) => ConstraintWitness (c (WriterT w m));
        selfReadable :: forall reader. ConstraintWitness (c (GenReadable c reader));
        selfWriterReadable :: forall w reader. ConstraintWitness (c (GenWriterReadable c w reader));
    };

    selfComposeReadable :: forall c f reader. (ReadableConstraint c,Monad f,Traversable f) => ConstraintWitness (c (Compose (GenReadable c reader) f));
    selfComposeReadable = case selfReadable @c @reader of
    {
        MkConstraintWitness -> case selfCompose @c @f @(GenReadable c reader) of
        {
            MkConstraintWitness -> MkConstraintWitness;
        };
    };

    instance ReadableConstraint Monad where
    {
        selfCompose = MkConstraintWitness;
        selfWriterT = MkConstraintWitness;
        selfReadable = MkConstraintWitness;
        selfWriterReadable = MkConstraintWitness;
    };

    instance ReadableConstraint MonadIO where
    {
        selfCompose = MkConstraintWitness;
        selfWriterT = MkConstraintWitness;
        selfReadable = MkConstraintWitness;
        selfWriterReadable = MkConstraintWitness;
    };

    $(return []);
    instance HasTypeInfo ReadableConstraint where
    {
        typeWitness = $(generateWitness [t|ReadableConstraint|]);
        typeName _ = "ReadableConstraint";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance ReadableConstraint Monad;
            instance ReadableConstraint MonadIO;
        |]);
    };

    instance ReadableConstraint c => MapReadable (GenWriterReadable c w) where
    {
        mapReadable rf (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> sbwt (mapMutableRead rf sa) wm;
        mapReadableF (rff :: ReadFunctionF f ra rb) (MkWriterReadable sbwt) = MkWriterReadable $ \(sa :: MutableRead m ra) wm -> getCompose $ case selfCompose @c @f @m of
        {
            MkConstraintWitness -> sbwt (mapStructureF rff sa) (fmap (MkCompose . fmap pure) wm);
        };
    };

    writerToReadable :: forall c w reader. ReadableConstraint c => GenWriterReadable c w reader () -> GenReadable c reader [w];
    writerToReadable (MkWriterReadable swma) = MkReadable $ \(s :: forall t. reader t -> m t) -> case selfWriterT @c @[w] @m of
    {
        MkConstraintWitness -> execWriterT $ swma (fmap lift s) (tell . pure);
    };
}
