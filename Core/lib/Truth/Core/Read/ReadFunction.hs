module Truth.Core.Read.ReadFunction where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.FullReader;
    import Truth.Core.Read.Readable;
    import Truth.Core.Read.WriterReadable;


    type ReadFunction c readera readerb = forall t. readerb t -> Readable c readera t;
    type IOReadFunction readera readerb = ReadFunction MonadIO readera readerb;
    type PureReadFunction readera readerb = ReadFunction Monad readera readerb;

    pureToReadFunction :: PureReadFunction readera readerb -> ReadFunction c readera readerb;
    pureToReadFunction rf rb = pureToReadable $ rf rb;

    mapMutableRead :: forall c m ra rb. (Monad m,c m) => ReadFunction c ra rb -> MutableRead m ra -> MutableRead m rb;
    mapMutableRead rfab sma rbt = unReadable (rfab rbt) sma;

    mapMutableReadW :: forall c m ra rb. (Monad m,c m) => ReadFunction c ra rb -> MutableReadW m ra -> MutableReadW m rb;
    mapMutableReadW rf (MkMutableReadW mr) = MkMutableReadW $ mapMutableRead rf mr;

    composeReadFunction :: forall c ra rb rc. ReadableConstraint c => ReadFunction c rb rc -> ReadFunction c ra rb -> ReadFunction c ra rc;
    composeReadFunction = case selfReadable @c @ra of
    {
        MkConstraintWitness -> mapMutableRead;
    };

    makeReadFunction :: (Reader rb) => Readable c ra (ReaderSubject rb) -> ReadFunction c ra rb;
    makeReadFunction = readFromM;

    simpleReadFunction :: (FullReader c ra,Reader rb) => (ReaderSubject ra -> ReaderSubject rb) -> ReadFunction c ra rb;
    simpleReadFunction ab = makeReadFunction (fmap ab fromReader);

    convertReadFunction :: (FullReader c ra,Reader rb,ReaderSubject ra ~ ReaderSubject rb) => ReadFunction c ra rb;
    convertReadFunction = simpleReadFunction id;

    fromReadFunction :: (Reader ra,PureFullReader rb) => PureReadFunction ra rb -> ReaderSubject ra -> ReaderSubject rb;
    fromReadFunction rf = fromPureReadable (mapReadable rf pureFromReader);

    mapGenReadable :: forall c ra rb t. ReadableConstraint c => ReadFunction c ra rb -> Readable c rb t -> Readable c ra t;
    mapGenReadable rf (MkReadable srbmt) = case selfReadable @c @ra of
    {
        MkConstraintWitness -> srbmt $ \rt -> rf rt;
    };

    fromReadFunctionM :: forall c m ra rb. (Monad m,ReadableConstraint c,c m,Reader ra,FullReader c rb) => ReadFunction c ra rb -> m (ReaderSubject ra) -> m (ReaderSubject rb);
    fromReadFunctionM rf mra = unReadable (mapGenReadable rf fromReader) $ readFromM mra;

    readFunctionStateT :: (Monad m,PureFullReader r) => PureReadFunction r r -> StateT (ReaderSubject r) m ();
    readFunctionStateT rf = StateT $ \oldval -> return ((),fromReadFunction rf oldval);

    type PureReadFunctionF f readera readerb = forall t. readerb t -> PureReadable readera (f t);

    mapStructureF :: Monad m => PureReadFunctionF f ra rb -> MutableRead m ra -> MutableRead (Compose m f) rb;
    mapStructureF rff sa rbt = Compose $ unReadable (rff rbt) sa;

    composeReadFunctionF :: PureReadFunctionF f rb rc -> PureReadFunction ra rb -> PureReadFunctionF f ra rc;
    composeReadFunctionF rbc rab rct = mapReadable rab (rbc rct);


    class MapReadable readable where
    {
        mapReadable :: PureReadFunction ra rb -> readable rb t -> readable ra t;
        mapReadableF :: (Monad f,Traversable f) => PureReadFunctionF f ra rb -> readable rb t -> readable ra (f t);
    };

    instance HasTypeInfo MapReadable where
    {
        typeWitness = $(generateWitness [t|MapReadable|]);
        typeName _ = "MapReadable";
    };

    instance ReadableConstraint c => MapReadable (Readable c) where
    {
        mapReadable (rf :: PureReadFunction ra rb) (MkReadable srbmt) = case selfReadable @c @ra of
        {
            MkConstraintWitness -> srbmt $ \rt -> pureToReadable $ rf rt;
        };
        mapReadableF (rff :: PureReadFunctionF f ra rb) (MkReadable srbmt) = case selfComposeReadable @c @f @ra of
        {
            MkConstraintWitness -> getCompose $ srbmt $ \rt -> Compose $ pureToReadable $ rff rt;
        };
    };


    class ReadableConstraint c where
    {
        selfCompose :: forall f m. (Monad f,Traversable f,c m) => ConstraintWitness (c (Compose m f));
        selfWriterT :: forall w m. (Monad m,Monoid w,c m) => ConstraintWitness (c (WriterT w m));
        selfReadable :: forall reader. ConstraintWitness (c (Readable c reader));
        selfWriterReadable :: forall w reader. ConstraintWitness (c (WriterReadable c w reader));
    };

    selfComposeReadable :: forall c f reader. (ReadableConstraint c,Monad f,Traversable f) => ConstraintWitness (c (Compose (Readable c reader) f));
    selfComposeReadable = case selfReadable @c @reader of
    {
        MkConstraintWitness -> case selfCompose @c @f @(Readable c reader) of
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

    instance ReadableConstraint c => MapReadable (WriterReadable c w) where
    {
        mapReadable rf (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> sbwt (mapMutableRead rf sa) wm;
        mapReadableF (rff :: PureReadFunctionF f ra rb) (MkWriterReadable sbwt) = MkWriterReadable $ \(sa :: MutableRead m ra) wm -> getCompose $ case selfCompose @c @f @m of
        {
            MkConstraintWitness -> sbwt (mapStructureF rff sa) (fmap (Compose . fmap pure) wm);
        };
    };

    writerToReadable :: forall c w reader. ReadableConstraint c => WriterReadable c w reader () -> Readable c reader [w];
    writerToReadable (MkWriterReadable swma) = MkReadable $ \(s :: forall t. reader t -> m t) -> case selfWriterT @c @[w] @m of
    {
        MkConstraintWitness -> execWriterT $ swma (fmap lift s) (tell . pure);
    };
}
