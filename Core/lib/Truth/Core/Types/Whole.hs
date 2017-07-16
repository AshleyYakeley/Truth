module Truth.Core.Types.Whole where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data WholeReader (a :: *) (t :: *) where
    {
        ReadWhole :: forall t. WholeReader t t;
    };

    instance Reader (WholeReader a) where
    {
        type ReaderSubject (WholeReader a) = a;
        readFrom msubj ReadWhole = msubj;
    };

    instance IOFullReader (WholeReader a);
    instance FullReader (WholeReader a) where
    {
        fromReader = readable ReadWhole;
    };

    $(return []);
    instance HasInfo WholeReader where
    {
        info = mkSimpleInfo $(ionamedwitness[t|WholeReader|]) [$(declInfo [d|
            instance Reader (WholeReader a) where
            {
                type ReaderSubject (WholeReader a) = a;
            };
            instance FullReader (WholeReader a);
        |])];
    };

    wholeMutableRead :: m a -> MutableRead m (WholeReader a);
    wholeMutableRead ma ReadWhole = ma;

    newtype WholeReaderEdit (reader :: * -> *) = MkWholeEdit (ReaderSubject reader);

    instance Floating (WholeReaderEdit reader) (WholeReaderEdit reader);

    instance (IOFullReader reader) => Edit (WholeReaderEdit reader) where
    {
        type EditReader (WholeReaderEdit reader) = reader;
        applyEdit (MkWholeEdit a) = readFromM (return a);
        invertEdit _ = do
        {
            a <- ioFromReader;
            return [MkWholeEdit a];
        };
    };

    instance (IOFullReader reader) => IOFullEdit (WholeReaderEdit reader) where
    {
        ioReplaceEdit = do
        {
            a <- readableToM ioFromReader;
            wrWrite $ MkWholeEdit a;
        };
    };

    instance (FullReader reader) => FullEdit (WholeReaderEdit reader) where
    {
        replaceEdit = do
        {
            a <- readableToM fromReader;
            wrWrite $ MkWholeEdit a;
        };
    };

    $(return []);
    instance HasInfo WholeReaderEdit where
    {
        info = mkSimpleInfo $(ionamedwitness[t|WholeReaderEdit|]) [$(declInfo [d|
            instance (IOFullReader reader) => Edit (WholeReaderEdit reader) where
            {
                type EditReader (WholeReaderEdit reader) = reader;
            };
            instance (FullReader reader) => FullEdit (WholeReaderEdit reader);
        |])];
    };

    type WholeEdit a = WholeReaderEdit (WholeReader a);

    wholeEditFunction :: (a -> b) -> EditFunction (WholeEdit a) (WholeEdit b);
    wholeEditFunction ab = MkEditFunction
    {
        editGet = simpleReadFunction ab,
        editUpdate = \(MkWholeEdit a) -> return $ pure $ MkWholeEdit $ ab a
    };

    wholeEditLens :: (Functor m) => Lens' m a b -> EditLens' m (WholeEdit a) (WholeEdit b);
    wholeEditLens lens = MkEditLens
    {
        editLensFunction = wholeEditFunction (lensGet lens),
        editLensPutEdit = \(MkWholeEdit newb) -> do
        {
            olda <- fromReader;
            let
            {
                newma = lensPutback lens newb olda;
                medita = fmap (pure . MkWholeEdit) newma;
            };
            return medita;
        }
    };
}
