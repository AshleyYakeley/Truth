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

    instance IOFullReader (NoReader ());
    instance FullReader (NoReader ()) where
    {
        fromReader = return ();
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
            instance FullReader (NoReader ());
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

    instance (IOFullReader reader,ReaderSubject reader ~ ()) => IOFullEdit (NoEdit reader) where
    {
        ioReplaceEdit = return ();
    };

    instance (FullReader reader,ReaderSubject reader ~ ()) => FullEdit (NoEdit reader) where
    {
        replaceEdit = return ();
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
            instance (FullReader reader,ReaderSubject reader ~ ()) => FullEdit (NoEdit reader);
        |]);
    };

    noEditFunction :: EditFunction (NoEdit (EditReader edit)) edit;
    noEditFunction = let
    {
        editGet = readable;
        editUpdate = never;
    } in MkEditFunction{..};

    noEditLens :: EditLens' Maybe (NoEdit (EditReader edit)) edit;
    noEditLens = let
    {
        editLensFunction = noEditFunction;
        editLensPutEdit _ = return Nothing;
    } in MkEditLens{..};
}
