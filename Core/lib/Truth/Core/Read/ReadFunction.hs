module Truth.Core.Read.ReadFunction where
{
    import Truth.Core.Import;
    import Truth.Core.Read.Reader;
    import Truth.Core.Read.FullReader;
    import Truth.Core.Read.Readable;


    type ReadFunction readera readerb = forall t. readerb t -> Readable readera t;

    mapStructure :: forall m ra rb. Monad m => ReadFunction ra rb -> Structure m ra -> Structure m rb;
    mapStructure rfab sma rbt = unReadable (rfab rbt) sma;

    composeReadFunction :: ReadFunction rb rc -> ReadFunction ra rb -> ReadFunction ra rc;
    composeReadFunction = mapStructure;

    makeReadFunction :: (Reader rb) => Readable ra (ReaderSubject rb) -> ReadFunction ra rb;
    makeReadFunction = readFromM;

    simpleReadFunction :: (FullReader ra,Reader rb) => (ReaderSubject ra -> ReaderSubject rb) -> ReadFunction ra rb;
    simpleReadFunction ab = makeReadFunction (fmap ab fromReader);

    convertReadFunction :: (FullReader ra,Reader rb,ReaderSubject ra ~ ReaderSubject rb) => ReadFunction ra rb;
    convertReadFunction = simpleReadFunction id;

    fromReadFunction :: (Reader ra,FullReader rb) => ReadFunction ra rb -> ReaderSubject ra -> ReaderSubject rb;
    fromReadFunction rf = fromReadable (mapReadable rf fromReader);


    type CleanReadFunction ra rb = Structure ra rb;

    cleanReadFunction :: CleanReadFunction ra rb -> ReadFunction ra rb;
    cleanReadFunction rbtrat rbt = MkReadable (\ratmt -> ratmt (rbtrat rbt));

    fromCleanReadFunction :: (Reader ra,FullReader rb) => CleanReadFunction ra rb -> ReaderSubject ra -> ReaderSubject rb;
    fromCleanReadFunction rbtrat a = runIdentity (unReadable fromReader (Identity . (readFrom a) . rbtrat));

    type ReadFunctionF f readera readerb = forall t. readerb t -> Readable readera (f t);

    mapStructureF :: Monad m => ReadFunctionF f ra rb -> Structure m ra -> Structure (Compose m f) rb;
    mapStructureF rff sa rbt = MkCompose $ unReadable (rff rbt) sa;

    class MapReadable readable where
    {
        mapReadable :: ReadFunction ra rb -> readable rb t -> readable ra t;
        mapReadableF :: (Monad f,Traversable f) => ReadFunctionF f ra rb -> readable rb t -> readable ra (f t);
    };

    mapCleanReadable :: MapReadable readable => CleanReadFunction ra rb -> readable rb t -> readable ra t;
    mapCleanReadable f = mapReadable (cleanReadFunction f);

    instance MapReadable Readable where
    {
        mapReadable rf (MkReadable srbmt) = srbmt rf;
        mapReadableF rff (MkReadable srbmt) = getCompose $ srbmt $ MkCompose . rff;
    };
}
