module Truth.Edit.WholeEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;


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
            instance Reader (WholeReader a);
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
            a <- fromReader;
            return [MkWholeEdit a];
        };
    };

    $(return []);
    instance HasInfo WholeEdit where
    {
        info = mkSimpleInfo $(iowitness[t|WholeEdit|]) [$(declInfo [d|
            instance (FullReader reader) => Edit (WholeEdit reader);
            instance (FullReader reader) => FullEdit (WholeEdit reader);
        |])];
    };
}
