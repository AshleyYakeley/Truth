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

    instance FullReader (WholeReader a) where
    {
        fromReader = readable ReadWhole;
    };

    $(return []);
    instance HasInfo WholeReader where
    {
        info = mkSimpleInfo $(iowitness[t|WholeReader|]) [$(declInfo [d|
            instance Reader (WholeReader a) where
            {
                type ReaderSubject (WholeReader a) = a;
            };
            instance FullReader (WholeReader a);
        |])];
    };

    newtype WholeEdit (reader :: * -> *) = MkWholeEdit (ReaderSubject reader);

    instance Floating (WholeEdit reader) (WholeEdit reader);

    instance (FullReader reader) => Edit (WholeEdit reader) where
    {
        type EditReader (WholeEdit reader) = reader;
        applyEdit (MkWholeEdit a) = readFromM (return a);
        invertEdit _ = do
        {
            a <- fromReader;
            return [MkWholeEdit a];
        };
    };

    instance (FullReader reader) => FullEdit (WholeEdit reader) where
    {
        replaceEdit = do
        {
            a <- readableToM fromReader;
            wrWrite $ MkWholeEdit a;
        };
    };

    $(return []);
    instance HasInfo WholeEdit where
    {
        info = mkSimpleInfo $(iowitness[t|WholeEdit|]) [$(declInfo [d|
            instance (FullReader reader) => Edit (WholeEdit reader) where
            {
                type EditReader (WholeEdit reader) = reader;
            };
            instance (FullReader reader) => FullEdit (WholeEdit reader);
        |])];
    };

    wholeEditFunction :: (a -> b) -> EditFunction (WholeEdit (WholeReader a)) (WholeEdit (WholeReader b));
    wholeEditFunction ab = MkEditFunction
    {
        editGet = simpleReadFunction ab,
        editUpdate = \(MkWholeEdit a) -> return $ MkWholeEdit $ ab a
    };

    wholeEditLens :: (Functor m) => Lens' m a b -> EditLens' m (WholeEdit (WholeReader a)) (WholeEdit (WholeReader b));
    wholeEditLens lens = MkEditLens
    {
        editLensFunction = wholeEditFunction (lensGet lens),
        editLensPutEdit = \(MkWholeEdit newb) -> do
        {
            olda <- fromReader;
            let
            {
                newma = lensPutback lens newb olda;
                medita = fmap MkWholeEdit newma;
            };
            return medita;
        }
    };
}
