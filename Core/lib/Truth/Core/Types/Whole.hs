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

    instance GenFullReader c (WholeReader a) where
    {
        genFromReader = readable ReadWhole;
    };

    $(return []);
    instance HasTypeInfo WholeReader where
    {
        typeWitness = $(generateWitness [t|WholeReader|]);
        typeName _ = "WholeReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Reader (WholeReader a) where
            {
                type ReaderSubject (WholeReader a) = a;
            };
            instance GenFullReader c (WholeReader a);
        |]);
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

    instance (ReadableConstraint c,GenFullReader MonadIO reader,GenFullReader c reader) => GenFullEdit c (WholeReaderEdit reader) where
    {
        genReplaceEdit = case selfWriterReadable @c @(WholeReaderEdit reader) @reader of
        {
            MkConstraintWitness -> do
            {
                a <- readableToM @c genFromReader;
                wrWrite $ MkWholeEdit a;
            };
        };
    };

    $(return []);
    instance HasTypeInfo WholeReaderEdit where
    {
        typeWitness = $(generateWitness [t|WholeReaderEdit|]);
        typeName _ = "WholeReaderEdit";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (IOFullReader reader) => Edit (WholeReaderEdit reader) where
            {
                type EditReader (WholeReaderEdit reader) = reader;
            };
            instance (ReadableConstraint c,IOFullReader reader,GenFullReader c reader) => GenFullEdit c (WholeReaderEdit reader);
        |]);
    };

    type WholeEdit a = WholeReaderEdit (WholeReader a);

    wholeEditFunction :: forall c a b. (a -> b) -> GenFloatingEditFunction c () (WholeEdit a) (WholeEdit b);
    wholeEditFunction ab = MkFloatingEditFunction
    {
        floatingEditInitial = (),
        floatingEditGet = \() -> simpleReadFunction ab,
        floatingEditUpdate = \(MkWholeEdit a) curstate -> return (curstate,[MkWholeEdit $ ab a])
    };

    wholeEditLens :: forall c m a b. (Functor m) => Lens' m a b -> GenFloatingEditLens' c m () (WholeEdit a) (WholeEdit b);
    wholeEditLens lens = MkFloatingEditLens
    {
        floatingEditLensFunction = wholeEditFunction (lensGet lens),
        floatingEditLensPutEdit = \() (MkWholeEdit newb) -> do
        {
            olda <- genFromReader;
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
