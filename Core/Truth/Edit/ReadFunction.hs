module Truth.Edit.ReadFunction where
{
    import Truth.Edit.Read;
    import Truth.Edit.Import;

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

    makeReadFunction :: (Reader rb) => Readable ra (Subject rb) -> ReadFunction ra rb;
    makeReadFunction = readFromM;

    simpleReadFunction :: (FullReader ra,Reader rb) => (Subject ra -> Subject rb) -> ReadFunction ra rb;
    simpleReadFunction ab = makeReadFunction (fmap ab fromReader);

    convertReadFunction :: (FullReader ra,Reader rb,Subject ra ~ Subject rb) => ReadFunction ra rb;
    convertReadFunction = simpleReadFunction id;

    fromReadFunction :: (Reader ra,FullReader rb) => ReadFunction ra rb -> Subject ra -> Subject rb;
    fromReadFunction rf = fromReadable (mapReadable rf fromReader);

    type CleanReadFunction ra rb = Structure ra rb;

    cleanReadFunction :: CleanReadFunction ra rb -> ReadFunction ra rb;
    cleanReadFunction rbtrat rbt = MkReadable (\ratmt -> ratmt (rbtrat rbt));

    fromCleanReadFunction :: (Reader ra,FullReader rb) => CleanReadFunction ra rb -> Subject ra -> Subject rb;
    fromCleanReadFunction rbtrat a = runIdentity (unReadable fromReader (Identity . (readFrom a) . rbtrat));

    mapCleanReadable :: CleanReadFunction ra rb -> Readable rb t -> Readable ra t;
    mapCleanReadable f = mapReadable (cleanReadFunction f);
}
