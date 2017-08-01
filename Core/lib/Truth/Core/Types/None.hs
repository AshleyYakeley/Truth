module Truth.Core.Types.None where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    newtype NoReader (a :: *) (t :: *) = MkNoReader None deriving (Eq,Countable,Searchable);

    instance Finite (NoReader a t) where
    {
        allValues = [];
    };

    deriving instance Empty (NoReader a t);

    instance Reader (NoReader a) where
    {
        type ReaderSubject (NoReader a) = a;
        readFromM _ = never;
        readFrom _ = never;
    };

    instance GenFullReader c (NoReader ()) where
    {
        genFromReader = return ();
    };

    $(return []);
    instance HasTypeInfo NoReader where
    {
        typeWitness = $(generateWitness [t|NoReader|]);
        typeName _ = "NoReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Reader (NoReader a) where
            {
                type ReaderSubject (NoReader a) = a;
            };
            instance GenFullReader c (NoReader ());
        |]);
    };

    -- | Can't touch this.
    newtype NoEdit (reader :: * -> *) = MkNoEdit None deriving (Eq,Countable,Searchable);

    instance Finite (NoEdit reader) where
    {
        allValues = [];
    };

    deriving instance Empty (NoEdit reader);

    instance Floating (NoEdit reader) (NoEdit reader);

    instance (Reader reader) => Edit (NoEdit reader) where
    {
        type EditReader (NoEdit reader) = reader;
        applyEdit = never;
        invertEdit = never;
    };

    instance (GenFullReader c reader,ReaderSubject reader ~ ()) => GenFullEdit c (NoEdit reader) where
    {
        genReplaceEdit = return ();
    };

    $(return []);
    instance HasTypeInfo NoEdit where
    {
        typeWitness = $(generateWitness [t|NoEdit|]);
        typeName _ = "NoEdit";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (Reader reader) => Edit (NoEdit reader) where
            {
                type EditReader (NoEdit reader) = reader;
            };
            instance (GenFullReader c reader,ReaderSubject reader ~ ()) => GenFullEdit c (NoEdit reader);
        |]);
    };

    noEditFunction :: FloatingEditFunction () (NoEdit (EditReader edit)) edit;
    noEditFunction = let
    {
        floatingEditInitial = ();
        floatingEditGet _ = readable;
        floatingEditUpdate = never;
    } in MkFloatingEditFunction{..};

    noEditLens :: FloatingEditLens' Maybe () (NoEdit (EditReader edit)) edit;
    noEditLens = let
    {
        floatingEditLensFunction = noEditFunction;
        floatingEditLensPutEdit () _ = return Nothing;
    } in MkFloatingEditLens{..};
}
