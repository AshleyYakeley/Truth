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

    instance FullReader c (WholeReader a) where
    {
        fromReader = readable ReadWhole;
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

    instance (ReadableConstraint c,FullReader MonadIO reader,FullReader c reader) => FullEdit c (WholeReaderEdit reader) where
    {
        replaceEdit = case selfWriterReadable @c @(WholeReaderEdit reader) @reader of
        {
            MkConstraintWitness -> do
            {
                a <- readableToM @c fromReader;
                wrWrite $ MkWholeEdit a;
            };
        };
    };

    type WholeEdit a = WholeReaderEdit (WholeReader a);

    wholeEditFunction :: forall c a b. (a -> b) -> EditFunction c () (WholeEdit a) (WholeEdit b);
    wholeEditFunction ab = MkEditFunction
    {
        editInitial = (),
        editGet = \() -> simpleReadFunction ab,
        editUpdate = \(MkWholeEdit a) curstate -> return (curstate,[MkWholeEdit $ ab a])
    };

    wholeEditLens :: forall c m a b. (Functor m) => Lens' m a b -> EditLens' c m () (WholeEdit a) (WholeEdit b);
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
            return medita;
        }
    };

    instance (MonadOne f) => IsGeneralLens (Lens' f a b) where
    {
        type LensMonad (Lens' f a b) = f;
        type LensDomain (Lens' f a b) = WholeEdit a;
        type LensRange (Lens' f a b) = WholeEdit b;

        toGeneralLens' = toGeneralLens' . wholeEditLens @MonadIO;
    };

    instance (MonadOne m) => IsGeneralLens (Injection' m a b) where
    {
        type LensMonad (Injection' m a b) = m;
        type LensDomain (Injection' m a b) = WholeEdit a;
        type LensRange (Injection' m a b) = WholeEdit b;

        toGeneralLens' = toGeneralLens' . injectionLens;
    };

    instance IsGeneralLens (Bijection a b) where
    {
        type LensMonad (Bijection a b) = Identity;
        type LensDomain (Bijection a b) = WholeEdit a;
        type LensRange (Bijection a b) = WholeEdit b;

        toGeneralLens' = toGeneralLens' . bijectionInjection;
    };

    instance IsGeneralLens (Codec a b) where
    {
        type LensMonad (Codec a b) = Maybe;
        type LensDomain (Codec a b) = WholeEdit a;
        type LensRange (Codec a b) = WholeEdit (Maybe b);

        toGeneralLens' = toGeneralLens' . codecInjection;
    };
}
