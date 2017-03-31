module Truth.Edit.Read.ReadFunction where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read.Reader;
    import Truth.Edit.Read.FullReader;
    import Truth.Edit.Read.Readable;

    -- type ReadFunction readera readerb = forall t. readerb t -> Readable readera t;
    type ReadFunction readera readerb = Structure (Readable readera) readerb;

    readable :: ReadFunction reader reader;
    readable rt = MkReadable (\s -> s rt);

    mapStructure :: forall m ra rb. (Applicative m,Monad m) => ReadFunction ra rb -> Structure m ra -> Structure m rb;
    mapStructure rfab sma rbt = unReadable (rfab rbt) sma;

    composeReadFunction :: ReadFunction rb rc -> ReadFunction ra rb -> ReadFunction ra rc;
    composeReadFunction = mapStructure;

    mapReadable :: ReadFunction ra rb -> Readable rb t -> Readable ra t;
    mapReadable rrarb rrb = unReadable rrb rrarb;

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

    mapCleanReadable :: CleanReadFunction ra rb -> Readable rb t -> Readable ra t;
    mapCleanReadable f = mapReadable (cleanReadFunction f);

    type ReadFunctionF f readera readerb = forall t. readerb t -> Readable readera (f t);

    mapReadableF :: (Monad f,Traversable f) => ReadFunctionF f ra rb -> Readable rb t -> Readable ra (f t);
    mapReadableF rff (MkReadable smrbmt) = getCompose $ smrbmt $ \rbq -> MkCompose $ rff rbq;
}
