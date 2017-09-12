{-# OPTIONS -fno-warn-orphans #-}
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

    wholeMutableRead :: m a -> MutableRead m (WholeReader a);
    wholeMutableRead ma ReadWhole = ma;

    newtype WholeReaderEdit (reader :: * -> *) = MkWholeEdit (ReaderSubject reader);

    instance Floating (WholeReaderEdit reader) (WholeReaderEdit reader);

    instance (FullReader reader) => Edit (WholeReaderEdit reader) where
    {
        type EditReader (WholeReaderEdit reader) = reader;
        applyEdit (MkWholeEdit a) = readFromM (return a);
        invertEdit _ = do
        {
            a <- fromReader;
            return [MkWholeEdit a];
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

    type WholeEdit a = WholeReaderEdit (WholeReader a);

    wholeEditFunction :: forall a b. (a -> b) -> EditFunction () (WholeEdit a) (WholeEdit b);
    wholeEditFunction ab = MkEditFunction
    {
        editInitial = (),
        editGet = \() -> simpleReadFunction ab,
        editUpdate = \(MkWholeEdit a) curstate -> return (curstate,[MkWholeEdit $ ab a])
    };

    wholeEditLens :: forall m a b. (MonadOne m) => Lens' m a b -> EditLens () (WholeEdit a) (WholeEdit b);
    wholeEditLens lens = MkEditLens
    {
        editLensFunction = wholeEditFunction (lensGet lens),
        editLensPutEdit = \() (MkWholeEdit newb) -> do
        {
            olda <- fromReader;
            let
            {
                newma = lensPutback lens newb olda;
                medita = fmap (\a -> ((),[MkWholeEdit a])) newma;
            };
            return $ getMaybeOne medita;
        }
    };

    instance MonadOne m => IsGeneralLens (Lens' m a b) where
    {
        type LensDomain (Lens' m a b) = WholeEdit a;
        type LensRange (Lens' m a b) = WholeEdit b;

        toGeneralLens = toGeneralLens . wholeEditLens;
    };

    instance MonadOne m => IsGeneralLens (Injection' m a b) where
    {
        type LensDomain (Injection' m a b) = WholeEdit a;
        type LensRange (Injection' m a b) = WholeEdit b;

        toGeneralLens = toGeneralLens . injectionLens;
    };

    instance IsGeneralLens (Bijection a b) where
    {
        type LensDomain (Bijection a b) = WholeEdit a;
        type LensRange (Bijection a b) = WholeEdit b;

        toGeneralLens = toGeneralLens . bijectionInjection;
    };

    instance IsGeneralLens (Codec a b) where
    {
        type LensDomain (Codec a b) = WholeEdit a;
        type LensRange (Codec a b) = WholeEdit (Maybe b);

        toGeneralLens = toGeneralLens . codecInjection;
    };

    unitWholeObjectFunction :: ObjectFunction edit (WholeEdit ());
    unitWholeObjectFunction = constEditFunction ();

    pairWholeObjectFunction :: forall edit a b. ObjectFunction edit (WholeEdit a) -> ObjectFunction edit (WholeEdit b) -> ObjectFunction edit (WholeEdit (a,b));
    pairWholeObjectFunction (MkEditFunction () ga ua) (MkEditFunction () gb ub) = let
    {
        gab :: () -> ReadFunction (EditReader edit) (WholeReader (a,b));
        gab () ReadWhole = do
        {
            a <- ga () ReadWhole;
            b <- gb () ReadWhole;
            return (a,b);
        };

        lastm :: forall x. [x] -> Maybe x;
        lastm [] = Nothing;
        lastm [x] = Just x;
        lastm (_:xx) = lastm xx;

        uab edit () = do
        {
            ((),editas) <- ua edit ();
            ((),editbs) <- ub edit ();
            case (lastm editas,lastm editbs) of
            {
                (Nothing,Nothing) -> return ((),[]);
                (ma,mb) -> do
                {
                    a <- case ma of
                    {
                        Just (MkWholeEdit a) -> return a;
                        Nothing -> ga () ReadWhole;
                    };
                    b <- case mb of
                    {
                        Just (MkWholeEdit b) -> return b;
                        Nothing -> gb () ReadWhole;
                    };
                    return ((),[MkWholeEdit (a,b)]);
                };
            };
        };
    } in MkEditFunction () gab uab;
}
